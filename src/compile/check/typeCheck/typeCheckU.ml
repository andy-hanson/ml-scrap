open N.Ty

let rec assert_exact(loc: Loc.t)(expected: ty)(actual: ty): unit =
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	match expected with
	| TPrimitive _ | Rt _ | GenVar _ ->
		if not (N.TyUU.equal expected actual) then fail()
	| Un {utys = expected_tys; _} ->
		begin match actual with
		| Un {utys = _actual_tys; _} ->
			U.todo()
		| TPrimitive _ | Rt _ ->
			(*TODO: this allows implicit conversion to union. Must mark down that we did so.*)
			(* A union type is guaranteed to only contain simple types. *)
			if not @@ ArrayU.exists expected_tys (N.TyUU.equal actual) then fail()
		| Ft _ ->
			fail()
		| GenRt _ | GenUn _ | GenFt _ | GenVar _ ->
			U.todo()
		end
	(* Functions match structurally, so ignore names. *)
	| Ft {ft_origin = _; return = expected_return; parameters = expected_params} ->
		begin match actual with
		| Ft {ft_origin = _; return = actual_return; parameters = actual_params} ->
			assert_exact loc expected_return actual_return;
			if (not @@ ArrayU.same_length expected_params actual_params) then fail(); (*TODO: error message*)
			ArrayU.iter_zip expected_params actual_params @@ fun (_, expected) (_, actual) ->
				assert_exact loc expected actual
		| _ ->
			fail()
		end
	| GenRt _ | GenUn _ | GenFt _ ->
		U.todo()

and assert_convert(loc: Loc.t)(expected: ty)(actual: ty): unit =
	(*TODO: Err.CantConvert*)
	let fail() = ErrU.raise loc @@ Err.NotExpectedTypeAndNoConversion(expected, actual) in
	match expected with
	| TPrimitive _ | Un _ ->
		assert_exact loc actual expected

	| Rt({properties = e_props; _} as e_rt) ->
		(*TODO: usually we won't need the conversion, so don't go checking so eagerly!*)
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

	| GenRt _ | GenUn _ | GenFt _ | GenVar _ ->
		U.todo()

let join(loc: Loc.t)(tys: ty array): ty =
	U.returning (Array.get tys 0) @@ fun ty0 ->
		ArrayU.iter tys @@ fun ty ->
			ErrU.check (N.TyUU.equal ty0 ty) loc @@ Err.CombineTypes(ty0, ty)
