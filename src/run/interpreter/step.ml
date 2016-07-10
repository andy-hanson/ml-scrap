let step(state: N.interpreter_state): bool =
	let goto idx = State.goto state idx; false in
	let next() = State.goto_next state; false in

	match State.cur_code state with
	| N.Call ->
		U.returning (next()) begin fun _ ->
			State.call state @@ State.pop state
		end

	| N.Cs parts ->
		let cased = State.peek state in
		let matching_case = ArrayU.find_map parts begin fun (typ, idx) ->
			OpU.op_if (Subsumes.f typ cased) @@ fun () -> idx
		end in
		goto @@ OpU.force matching_case

	| N.Const value ->
		State.push state value;
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

	| N.Partial arity ->
		let partially_applied =
			match State.pop state with
			| N.Fn f -> f
			| _ -> assert false in
		let partial_args = State.pop_n state arity in
		State.push state @@ N.Fn(N.PartialFn {N.partially_applied; N.partial_args});
		next()

	| N.Quote strings ->
		let interpolated = State.pop_n state @@ Array.length strings - 1 in
		let b = BatBuffer.create 16 in
		let output = BatBuffer.output_buffer b in
		BatBuffer.add_string b strings.(0);
		for i = 0 to Array.length interpolated - 1 do
			ValU.output output interpolated.(i);
			BatBuffer.add_string b strings.(i + 1)
		done;
		let result = N.v_string (BatBuffer.contents b) in

		State.push state result;
		next()

	| N.Check ->
		let checked = State.pop state in
		if not @@ ValU.bool_of checked then
			(*TODO Noze exception, not ocaml exeption*)
			raise U.TODO;
		State.push state N.v_void;
		next()

	| N.Nil ->
		assert false
