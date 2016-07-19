open N

let rt_eq({rname = rname_a; _}: rt)({rname = rname_b; _}: rt): bool =
	(*TODO: origin should be the same, not just the name!*)
	rname_a = rname_b

(* This is just used for `Un` member testing *)
let eq(a: ty)(b: ty): bool =
	match a with
	| TPrimitive _ ->
		a = b
	| Rt rt_a ->
		begin match b with
		| Rt rt_b -> rt_eq rt_a rt_b
		| _ -> false
		end
	| Un _ | Ft _ ->
		assert false
	| TyGen _ | TyVar _ | TyInst _ ->
		raise U.TODO

let rec assert_exact(loc: Loc.t)(expected: ty)(actual: ty): unit =
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	match expected with
	| TPrimitive _ ->
		(* These are all nominal types. *)
		if not (expected = actual) then fail()
	| Rt rt ->
		begin match actual with
		| Rt actual_rt ->
			if not (rt_eq rt actual_rt) then fail()
		| _ -> fail()
		end
	| Un {utys = expected_tys; _} ->
		begin match actual with
		| Un {utys = _actual_tys; _} ->
			raise U.TODO
		| TPrimitive _ | Rt _ ->
			(*TODO: this allows implicit conversion to union. Must mark down that we did so.*)
			(* A union type is guaranteed to only contain simple types. *)
			if not @@ ArrayU.exists expected_tys (eq actual) then fail()
		| Ft _ ->
			fail()
		| TyGen _ | TyVar _ | TyInst _ ->
			raise U.TODO
		end
	(* Functions match structurally, so ignore names. *)
	| Ft {N.fname = _; N.return = expected_return; N.parameters = expected_params} ->
		begin match actual with
		| Ft {N.fname = _; N.return = actual_return; N.parameters = actual_params} ->
			assert_exact loc expected_return actual_return;
			if (not @@ ArrayU.same_length expected_params actual_params) then fail(); (*TODO: error message*)
			ArrayU.iter_zip expected_params actual_params @@ fun (_, expected) (_, actual) ->
				assert_exact loc expected actual
		| _ ->
			fail()
		end
	| TyGen _ ->
		(* This is  the case where a variable is expected to be a generic function. *)
		raise U.TODO
	| TyVar _ ->
		if not (eq expected actual) then fail()
	| TyInst _ ->
		raise U.TODO

and assert_convert(loc: Loc.t)(expected: ty)(actual: ty): unit =
	(*TODO: Err.CantConvert*)
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	match expected with
	| TPrimitive _ | Un _ ->
		assert_exact loc actual expected

	| Rt({properties = e_props; _} as e_rt) ->
		begin match actual with
		| Rt({properties = a_props; _} as a_rt) ->
			(* Must have all the fields. *)
			ArrayU.iter e_props @@ fun (e_name, e_ty) ->
				let matching_prop_ty = ArrayU.find_map a_props @@ fun (a_name, a_ty) ->
					OpU.op_if (Sym.eq e_name a_name) @@ fun () -> a_ty in
				begin match matching_prop_ty with
				| Some a_ty ->
					assert_exact loc e_ty a_ty
				| None ->
					ErrU.raise loc @@ Err.CantConvertRtMissingProperty(e_rt, a_rt, e_name)
				end
		| _ ->
			fail()
		end

	| Ft _ ->
		(*TODO LATER: function conversion*)
		assert_exact loc actual expected

	| TyGen _ | TyVar _ | TyInst _ ->
		raise U.TODO

let join(loc: Loc.t)(tys: ty array): ty =
	U.returning (Array.get tys 0) @@ fun ty0 ->
		ArrayU.iter tys @@ fun ty ->
			ErrU.check (eq ty0 ty) loc @@ Err.CombineTypes(ty0, ty)
