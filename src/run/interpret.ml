type call_stack_entry = {
	fn: Code.fn;
	(* The fn's code *)
	code: Code.bytecode array;
	(* Index of the first local on the stack. Parameters come before this. *)
	stack_start_index: int;
	(* Index where we were in this fn before entering another one *)
	mutable code_idx: int
}
let entry_of(fn: Code.fn)(cur_stack_index: int): call_stack_entry = {
	fn = fn;
	code = fn.Code.code;
	stack_start_index = cur_stack_index;
	code_idx = 0
}

type interpreter_state = {
	data_stack: Val.t GoodStack.t;
	call_stack: call_stack_entry GoodStack.t;
	(* Currently executing fn *)
	mutable cur: call_stack_entry;
}

let peek(state: interpreter_state): Val.t =
	GoodStack.peek state.data_stack

let pop(state: interpreter_state): Val.t =
	GoodStack.pop state.data_stack

let pop_n(state: interpreter_state)(n: int): Val.t array =
	GoodStack.pop_n state.data_stack n

let push(state: interpreter_state)(value: Val.t): unit =
	GoodStack.push state.data_stack value

let call_builtin(state: interpreter_state)(builtin: Builtin.t): unit =
	let unary f =
		let a = pop state in
		push state (f a) in
	let binary f =
		let b = pop state in
		let a = pop state in
		push state (f a b) in
	let binary_int f =
		binary (fun a b -> Val.Int (f (ValU.int_of a) (ValU.int_of b))) in
	let binary_int_bool f =
		binary (fun a b -> Val.Bool (f (ValU.int_of a) (ValU.int_of b))) in
	match builtin with
	| Builtin.Not ->
		unary (fun a -> Val.Bool (not (ValU.bool_of a)))
	| Builtin.Less ->
		binary_int_bool (<)
	| Builtin.Add ->
		binary_int (+)
	| Builtin.Subtract ->
		binary_int (-)
	| Builtin.Times ->
		binary_int ( * )
	| Builtin.FloatToInt ->
		unary (fun f -> Val.Int (int_of_float (ValU.float_of f)))
	| _ ->
		failwith "not a fn, should not typecheck"

let cur_code(state: interpreter_state): Code.bytecode =
	state.cur.code.(state.cur.code_idx)

let step(state: interpreter_state): bool =
	let goto idx =
		state.cur.code_idx <- idx;
		false in
	let next () = goto (state.cur.code_idx + 1) in

	match cur_code state with
	| Code.Call fn ->
		(*TODO: neater *)
		GoodStack.push state.call_stack state.cur;
		(* When fn finishes, we want to return to the *next* bytecode. *)
		U.returning (next()) begin fun _ ->
			state.cur <- entry_of fn (GoodStack.size state.data_stack)
		end

	| Code.CallBuiltin b ->
		call_builtin state b;
		next()

	| Code.Case parts ->
		let cased = peek state in
		let matching_case = ArrayU.find_map parts begin fun (typ, idx) ->
			OpU.op_if (TypeU.subsumes typ cased) (fun () -> idx)
		end in
		goto matching_case

	| Code.Const value ->
		push state value;
		next()

	| Code.Construct record ->
		let properties = pop_n state (TypeU.rc_arity record) in
		push state (Val.Rc(record, properties));
		next()

	| Code.Drop ->
		ignore (pop state);
		next()

	| Code.Goto new_idx ->
		goto new_idx

	| Code.GotoIfFalse new_idx ->
		let cond = pop state in
		if ValU.bool_of cond then next() else goto new_idx

	| Code.Load stack_index ->
		let index = state.cur.stack_start_index + stack_index in
		let loaded = GoodStack.get state.data_stack index in
		push state loaded;
		(* push state (GoodStack.peek_by state.data_stack offset); *)
		next()

	| Code.Return ->
		(* Remove args from the stack, but leave return value. *)
		let return_value = pop state in
		Assert.equal (GoodStack.size state.data_stack) state.cur.stack_start_index OutputU.output_int;
		U.do_times (CodeU.fn_arity state.cur.fn) begin fun () ->
			ignore (pop state)
		end;
		push state return_value;

		begin match GoodStack.try_pop state.call_stack with
		| None ->
			(* Main fn exited, we're done *)
			true
		| Some entry ->
			state.cur <- entry;
			false
		end

	| Code.UnLet ->
		(* Stack effect: `... a b` -> `... b` *)
		let x = pop state in
		ignore (pop state);
		push state x;
		next()

(*TODO: use somewhere*)
let debug_step(state: interpreter_state): bool =
	let stack = state.data_stack in
	OutputU.printf "Stack: %a (start: %d)\n" (GoodStack.output ValU.output) stack state.cur.stack_start_index;
	OutputU.printf "Executing: %a\n" CodeU.output_bytecode (cur_code state);
	step state

let call_fn({Code.return_type; Code.params; _} as fn: Code.fn)(args: Val.t array): Val.t =
	ArrayU.iter_zip params args begin fun {Code.param_type; _} arg ->
		TypeU.assert_subsumes param_type arg
	end;
	let state = {
		data_stack = GoodStack.create();
		call_stack = GoodStack.create();
		cur = entry_of fn (CodeU.fn_arity fn)
	} in
	ArrayU.iter args (push state);
	while not (debug_step state) do () done;
	U.returning (GoodStack.peek state.data_stack) (TypeU.assert_subsumes return_type)
