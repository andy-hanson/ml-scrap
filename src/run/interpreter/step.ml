open N

(*TODO:MOVE*)
let case_test(ty: ty)(v: v): bool =
	match ty with
	(*TODO: TPrimitive and Rt together are TConcrete. (They have no subtypes.) This is useful because unions can only contain concrete types.*)
	| TPrimitive tp ->
		begin match v with
		| Primitive p -> tp == ValU.ty_of_primitive p
		| _ -> false
		end
	| Rt rt ->
		begin match v with
		| Rc (vrt, _) -> rt == vrt
		| _ -> false
		end
	| TyGen _ | TyVar _ | TyInst _ -> raise U.TODO
	| Un _ | Ft _ -> assert false (*TODO: ocaml type system should be more specific here*)


(*
and step_result =
	| NotDone
	| Done of v
	| AwaitingIo of v Lwt.t
	| AwaitingThread of thread
*)

let step(state: interpreter_state): step_result =
	let goto idx = State.goto state idx; NotDone in
	let next() = State.goto_next state; NotDone in

	match State.cur_code state with
	| Call ->
		U.returning (next()) @@ fun _ ->
			State.call state @@ State.pop state

	| Cs parts ->
		let cased = State.peek state in
		let matching_case = ArrayU.find_map parts @@ fun (ty, idx) ->
			OpU.op_if (case_test ty cased) @@ fun () -> idx in
		goto @@ OpU.force matching_case

	| Const value ->
		State.push state value;
		next()

	| CnvRc(rt, indices) ->
		let properties =
			match State.pop state with
			| Rc(_, properties) -> properties
			| _ -> assert false in
		let rc = Rc(rt, ArrayU.map indices @@ Array.get properties) in
		State.push state rc;
		next()

	| Destruct patterns ->
		let rec use_pattern(pattern: pattern)(value: v) =
			match pattern with
			| PSingle ->
				State.push state value
			| PDestruct patterns ->
				destruct patterns value
		and destruct(patterns: pattern array)(value: v) =
			match value with
			| Rc(_, properties) ->
				ArrayU.iter_zip patterns properties use_pattern
			| _ ->
				assert false in
		destruct patterns (State.pop state);
		next()

	| Drop ->
		ignore (State.pop state);
		next()

	| Dup ->
		State.push state (State.peek state);
		next()

	| GetProperty property_index ->
		let value =
			begin match State.pop state with
			| Rc(_, properties) ->
				properties.(property_index)
			| _ ->
				assert false
			end in
		State.push state value;
		next()

	| Goto new_idx ->
		goto new_idx

	| GotoIfFalse new_idx ->
		let cond = State.pop state in
		if ValU.bool_of cond then next() else goto new_idx

	| Load relative_index ->
		State.push state (State.load state relative_index);
		next()

	| Return ->
		(* Remove args from the stack, but leave return value. *)
		let return_value = State.pop state in
		State.assert_data_stack_back_to_function_start state;
		if State.pop_fn state then begin
			U.do_times (ValU.fn_arity @@ State.cur_fn state) begin fun () ->
				ignore (State.pop state)
			end;
			State.push state return_value;
			NotDone
		end else
			Done return_value

	| UnLet n ->
		(* Stack effect: `... a b` -> `... b` *)
		State.un_let state n;
		next()

	| Partial arity ->
		let partially_applied =
			match State.pop state with
			| Fn f -> f
			| _ -> assert false in
		let partial_args = State.pop_n state arity in
		State.push state @@ Fn(PartialFn {partially_applied; partial_args});
		next()

	| Quote strings ->
		let interpolated = State.pop_n state @@ Array.length strings - 1 in
		let b = BatBuffer.create 16 in
		let output = BatBuffer.output_buffer b in
		BatBuffer.add_string b strings.(0);
		for i = 0 to Array.length interpolated - 1 do
			ValU.output output interpolated.(i);
			BatBuffer.add_string b strings.(i + 1)
		done;
		let result = v_string (BatBuffer.contents b) in

		State.push state result;
		next()

	| Check ->
		let checked = State.pop state in
		if not @@ ValU.bool_of checked then
			(*TODO Noze exception, not ocaml exeption*)
			raise U.TODO;
		State.push state v_void;
		next()

	| Nil ->
		assert false
