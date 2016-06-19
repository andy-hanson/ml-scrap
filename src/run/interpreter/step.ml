let step(state: State.t): bool =
	let goto idx = State.goto state idx; false in
	let next() = State.goto_next state; false in
	let exec_builtin {Val.exec; _} =
		State.push state (exec (fun () -> State.pop state)) in

	match State.cur_code state with
	| Val.CallStatic fn ->
		(* When fn finishes, we want to return to the *next* byteVal. *)
		U.returning (next()) begin fun _ ->
			State.push_fn state fn
		end

	| Val.CallBuiltin b ->
		exec_builtin b;
		next()

	| Val.CallLambda ->
		begin match State.pop state with
		| Val.BuiltinFn b ->
			exec_builtin b;
			next()
		| Val.Fn(_closure_fn, _closure_params) ->
			raise U.TODO
		| _ ->
			assert false
		end

	| Val.Case parts ->
		let cased = State.peek state in
		let matching_case = ArrayU.find_map parts begin fun (typ, idx) ->
			OpU.op_if (Subsumes.f typ cased) (fun () -> idx)
		end in
		goto matching_case

	| Val.Const value ->
		State.push state value;
		next()

	| Val.Construct record ->
		let properties = State.pop_n state (TypeU.rc_arity record) in
		State.push state (Val.Rc(record, properties));
		next()

	| Val.Drop ->
		ignore (State.pop state);
		next()

	| Val.Goto new_idx ->
		goto new_idx

	| Val.GotoIfFalse new_idx ->
		let cond = State.pop state in
		if ValU.bool_of cond then next() else goto new_idx

	| Val.Load relative_index ->
		State.push state (State.load state relative_index);
		next()

	| Val.Return ->
		(* Remove args from the stack, but leave return value. *)
		let return_value = State.pop state in
		State.assert_data_stack_back_to_function_start state;
		U.do_times (ValU.fn_arity (State.cur_fn state)) begin fun () ->
			ignore (State.pop state)
		end;
		State.push state return_value;
		State.pop_fn state

	| Val.UnLet ->
		(* Stack effect: `... a b` -> `... b` *)
		State.un_let state;
		next()

let debug_step(noze: Noze.t)(file_name: FileIO.file_name)(state: State.t): bool =
	State.debug_print noze file_name state;
	step state
