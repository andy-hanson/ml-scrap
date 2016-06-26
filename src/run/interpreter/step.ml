let step(state: State.t): bool =
	let goto idx = State.goto state idx; false in
	let next() = State.goto_next state; false in
	let exec_builtin {N.exec; _} =
		State.push state (exec @@ fun () -> State.pop state) in

	match State.cur_code state with
	| N.CallStatic fn ->
		(* When fn finishes, we want to return to the *next* byteVal. *)
		U.returning (next()) begin fun _ ->
			State.push_fn state fn
		end

	| N.CallBuiltin b ->
		exec_builtin b;
		next()

	| N.CallLambda ->
		begin match State.pop state with
		| N.BuiltinFn b ->
			exec_builtin b;
			next()
		| N.Fn(_fn) ->
			raise U.TODO
		(*TODO:KILL| N.Cn(_cn) ->
			raise U.TODO*)
		| N.World ->
			State.push state @@ CallWorld.f @@ State.pop state;
			next()
		| N.Bool _ | N.Int _ | N.Float _ | N.Rc _ | N.Void ->
			assert false
		end

	| N.Case parts ->
		let cased = State.peek state in
		let matching_case = ArrayU.find_map parts begin fun (typ, idx) ->
			OpU.op_if (Subsumes.f typ cased) @@ fun () -> idx
		end in
		goto matching_case

	| N.Const value ->
		State.push state value;
		next()

	| N.Construct rt ->
		let properties = State.pop_n state @@ TypeU.rt_arity rt in
		State.push state @@ N.Rc(rt, properties);
		next()

	| N.Drop ->
		ignore (State.pop state);
		next()

	| N.Goto new_idx ->
		goto new_idx

	| N.GotoIfFalse new_idx ->
		let cond = State.pop state in
		if ValU.bool_of cond then next() else goto new_idx

	| N.Load relative_index ->
		State.push state (State.load state relative_index);
		next()

	| N.Return ->
		(* Remove args from the stack, but leave return value. *)
		let return_value = State.pop state in
		State.assert_data_stack_back_to_function_start state;
		U.do_times (ValU.fn_arity @@ State.cur_fn state) begin fun () ->
			ignore (State.pop state)
		end;
		State.push state return_value;
		State.pop_fn state

	| N.UnLet ->
		(* Stack effect: `... a b` -> `... b` *)
		State.un_let state;
		next()

	| N.Nil ->
		assert false

let debug_step(noze: Noze.t)(file_name: FileIO.file_name)(state: State.t): bool =
	State.debug_print noze file_name state;
	step state
