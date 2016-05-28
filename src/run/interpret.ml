
type call_stack_entry = {
	fn: Code.func;
	(* The function's code *)
	code: Code.bytecode array;
	(* Index where we were in this function before entering another one *)
	mutable code_idx: int
}
let entry_of(fn: Code.func): call_stack_entry = {
	fn = fn;
	code = fn.Code.code;
	code_idx = 0
}

type interpreter_state = {
	data_stack: Val.t GoodStack.t;
	(* TODO: this needs to store previous functions' code_idx_es too! *)
	call_stack: call_stack_entry GoodStack.t;
	(* Currently executing function *)
	mutable cur: call_stack_entry;
}

let pop(state: interpreter_state): Val.t =
	GoodStack.pop state.data_stack

let pop_n(state: interpreter_state)(n: int): Val.t array =
	Array.init n (fun _ -> pop state)

let push(state: interpreter_state)(value: Val.t): unit =
	GoodStack.push state.data_stack value

let call_builtin(state: interpreter_state)(builtin: Builtins.builtin): unit =
	let unary f =
		let a = pop state in
		push state (f a) in
	let binary f =
		let b = pop state in
		let a = pop state in
		push state (f a b) in
	let binary_int f =
		binary (fun a b -> Val.Int (f (Val.cast_as_int a) (Val.cast_as_int b))) in
	let binary_int_bool f =
		binary (fun a b -> Val.Bool (f (Val.cast_as_int a) (Val.cast_as_int b))) in
	match builtin with
	| Builtins.Not ->
		unary (fun a -> Val.Bool (not (Val.cast_as_bool a)))
	| Builtins.Less ->
		binary_int_bool (<)
	| Builtins.Add ->
		binary_int (+)
	| Builtins.Subtract ->
		binary_int (-)
	| Builtins.Times ->
		binary_int ( * )
	| _ ->
		failwith "not a function, should not typecheck"

let cur_code(state: interpreter_state): Code.bytecode =
	Array.get state.cur.code state.cur.code_idx

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
		U.returning (next()) (fun _ -> state.cur <- entry_of fn)

	| Code.CallBuiltin b ->
		call_builtin state b;
		next()

	| Code.Const value ->
		push state value;
		next()

	| Code.Construct record ->
		let properties = pop_n state (Type.record_arity record) in
		push state (Val.Record(record, properties));
		next()

	| Code.Drop ->
		ignore (pop state);
		next()

	| Code.Goto new_idx ->
		goto new_idx

	| Code.GotoIfFalse new_idx ->
		let cond = pop state in
		if Val.cast_as_bool cond then next() else goto new_idx

	| Code.Load offset ->
		push state (GoodStack.peek_by state.data_stack offset);
		next()

	| Code.Return ->
		begin match GoodStack.try_pop state.call_stack with
		| None ->
			(* Main function exited, we're done *)
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

let debug_step(state: interpreter_state): bool =
	let stack = state.data_stack in
	Batteries.Printf.printf "Stack: %a\n" (GoodStack.output Val.output) stack;
	Batteries.Printf.printf "Executing: %a\n" Code.output_code (cur_code state);
	step state

let call_fn(fn: Code.func)(args: Val.t array): Val.t =
	(* TODO: typecheck args *)
	let state = {
		data_stack = GoodStack.create();
		call_stack = GoodStack.create();
		cur = entry_of fn
	} in
	Array.iter (push state) args;
	while not (debug_step state) do () done;
	(GoodStack.peek state.data_stack)

let test_interpret(): Val.t =
	let f = {
		(* Code.ast = Ast.DeclVal(Loc.start, { Symbol.name = "dummy" }, Ast.Fn(Ast.Signature(Loc.start, Ast.DumbTyp, [||]), Ast.Literal (Val.Int 0))); *)
		Code.fname = { Symbol.name = "dummy" };
		Code.params = [| |];
		Code.code = [| Code.CallBuiltin Builtins.Add; Code.Return |]
	} in
	call_fn f [| Val.Int 1; Val.Int 2 |]


(* let test_interpret(): Val.t =
	let codes = [| Const (Val.Int 1); Const (Val.Int 2); CallPrim Add |] in
	let state = {
		data_stack = new GoodStack.t;
		call_stack = new GoodStack.t;
		code = codes;
		code_idx = 0
	} in
	step state;
	step state;
	step state;
	state.data_stack#peek
*)
