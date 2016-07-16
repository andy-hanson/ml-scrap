open N

(* This is just for `case` parameters *)
let eq(a: ty)(b: ty): bool =
	a = b

let rec assert_eq(loc: Loc.t)(expected: ty)(actual: ty): unit =
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	match expected with
	| TPrimitive _ | Rt _ ->
		(* These are all nominal types. *)
		if not (eq expected actual) then fail()
	| Un _ ->
		(*TODO: Allow parameter swapping. Actually, this part may never be called, because we always do union subtyping...*)
		raise U.TODO
	| Ft {N.fname = _; N.return = expected_return; N.parameters = expected_params} ->
		begin match actual with
		| Ft {N.fname = _; N.return = actual_return; N.parameters = actual_params} ->
			(*TODO: suggest upcasting on failure*)
			assert_eq loc expected_return actual_return;
			if (Array.length expected_params != Array.length actual_params) then fail(); (*TODO: error message*)
			ArrayU.iter_zip expected_params actual_params begin fun (_, expected) (_, actual) ->
				assert_eq loc expected actual
			end
		| _ ->
			fail()
		end

(*TODO: Pretty sure this should be the same thing as assert_upcast*)
let rec assert_convert(loc: Loc.t)(expected: ty)(actual: ty): unit =
	(*TODO: Err.CantConvert*)
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	match expected with
	| TPrimitive _ ->
		assert_eq loc actual expected
	| Rt({properties = e_props; _} as e_rt) ->
		begin match actual with
		| Rt({properties = a_props; _} as a_rt) ->
			(* Must have all the fields. *)
			ArrayU.iter e_props begin fun (e_name, e_ty) ->
				let matching_prop_ty = ArrayU.find_map a_props begin fun (a_name, a_ty) ->
					OpU.op_if (Sym.eq e_name a_name) @@ fun () -> a_ty
				end in
				match matching_prop_ty with
				| Some a_ty ->
					assert_exact loc e_ty a_ty
				| None ->
					ErrU.raise loc @@ Err.CantConvertRtMissingProperty(e_rt, a_rt, e_name)
			end
		| _ ->
			fail()
		end
	| Un _ ->
		assert_exact loc expected actual
	| Ft _ ->
		assert_upcast loc expected actual

and assert_upcast(loc: Loc.t)(expected: ty)(actual: ty): unit =
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	let foo =
		match expected with
		| Ft f -> f
		| _ -> raise U.TODO (*TODO: error?*) in
	let {fname = _; return = expected_return_ty; parameters = expected_parameters} = foo in
	begin match actual with
	| Ft {fname = _; return = actual_return_ty; parameters = actual_parameters} ->
		assert_exact loc expected_return_ty actual_return_ty;
		(* Functions are contravariant in parameter types *)
		ArrayU.iter_zip expected_parameters actual_parameters begin fun (_, expected_param_ty) (_, actual_param_ty) ->
			(*TODO: better error message for this case*)
			assert_exact loc actual_param_ty expected_param_ty
		end
	| _ ->
		fail()
	end

(*TODO:neater*)
and assert_exact(loc: Loc.t)(expected: ty)(actual: ty): unit =
	(*TODO: Err.NoExact*)
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	match expected with
	| TPrimitive _ ->
		assert_convert loc expected actual
	| Rt rt ->
		begin match actual with
		| Rt actual_rt ->
			(*TODO: warn if an upcast would be a good idea*)
			if not (rt == actual_rt) then fail()
		| _ -> fail()
		end
	| Un {utys = expected_tys; _} ->
		begin match actual with
		| Un {utys = _actual_tys; _} ->
			raise U.TODO
		| TPrimitive _ | Rt _ ->
			(* A union type is guaranteed to only contain simple types. *)
			if not @@ ArrayU.exists expected_tys ((=) actual) then fail()
		| Ft _ ->
			fail()
		end
	| Ft _ ->
		(* Don't allow implicit upcasting of function parameters *)
		assert_eq loc expected actual

let join(loc: Loc.t)(tys: ty array): ty =
	(*
	TODO: actual join algorithm
	This will only be used in the case of type inference.
	We should require that any union types be explicitly annotated, so don't have to worry about those.
	Some for joining functions of different types.
	ACTUALLY, don't even *join*, just do what we're doing here...
	error message should mention that an explicit annotation could help...
	*)
	let t = Array.get tys 0 in
	ArrayU.iter tys begin fun ty ->
		ErrU.check (eq t ty) loc @@ Err.CombineTypes(t, ty)
	end;
	t
