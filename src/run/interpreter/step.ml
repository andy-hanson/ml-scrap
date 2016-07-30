open N.V
open N.Ty
open N.Code
open N.Run

(*TODO:MOVE*)
let case_test(ty: ty)(v: v): bool =
	match ty with
	(*TODO: TPrimitive and Rt together are TConcrete. (They have no subtypes.) This is useful because unions can only contain concrete types.*)
	| TPrimitive tp ->
		begin match v with
		| Primitive p -> N.TyUU.primitive_equal tp @@ ValU.ty_of_primitive p
		| _ -> false
		end
	| Rt rt ->
		begin match v with
		| Rc (vrt, _) -> N.TyUU.rt_equal rt vrt
		| _ -> false
		end
	| Un _ | Ft _ | GenRt _ | GenUn _ | GenFt _ | GenVar _ -> assert false (*TODO: ocaml type system should be more specific here*)





let debug_print(state: interpreter_state): unit =
	let {cur = {stack_start_index; _}; data_stack; _} = state in
	OutputU.printf "Stack: %a (stack_start_index: %i)\n"
		(MutArray.output_with_max 3 ValOut.output) data_stack
		stack_start_index;
	(*let lc_loc = Noze.lc_loc noze full_path @@ State.cur_loc state in*)
	OutputU.printf "Executing: %a\n"
		ValOut.output_bytecode (State.cur_code state)
		(*Path.output full_path
		Loc.output_lc_loc lc_loc*)






let step(state: interpreter_state): step_result =
	if false then debug_print state;

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
		State.drop_to_start_of_fn state;
		if State.pop_fn state then begin
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
			ValOut.output output interpolated.(i);
			BatBuffer.add_string b strings.(i + 1)
		done;
		let result = ValU.v_string @@ BatBuffer.contents b in

		State.push state result;
		next()

	| Check ->
		let checked = State.pop state in
		if not @@ ValU.bool_of checked then
			(*TODO Noze exception, not ocaml exeption*)
			U.todo();
		State.push state ValU.v_void;
		next()

	| Nil ->
		assert false
