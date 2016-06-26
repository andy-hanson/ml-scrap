open N

let rec f(t: ty)(v: v): bool =
	match t with
	| Any ->
		true
	| TBool ->
		begin match v with
		| Bool _ -> true
		| _ -> false
		end
	| TFloat ->
		begin match v with
		| Float _ -> true
		| _ -> false
		end
	| TInt ->
		begin match v with
		| Int _ -> true
		| _ -> false
		end
	| TVoid ->
		v = Void
	| N.Ft _ ->
		begin match v with
		| Fn _ ->
			TypeU.is_subtype t (ValU.type_of v)
		| BuiltinFn {builtin_ft; _} ->
			TypeU.is_subtype t @@ Ft builtin_ft
		| World ->
			raise U.TODO
		| Bool _ | Int _ | Float _ | Void | Rc _ ->
			false
		end
	| Rt rc ->
		begin match v with
		| Rc(val_rc, _) -> rc == val_rc
		| _ -> false
		end
	| Un {utypes; _} ->
		ArrayU.exists utypes @@ fun t -> f t v
	| Ct {ct_cases = _; _} as ct ->
		(*TODO: v must be a Fn or BuiltinFn whose type is_subtype, much like the Ft case (so share code!)*)
		begin match v with
		| Fn _ ->
			raise U.TODO
		| BuiltinFn _ ->
			raise U.TODO
		| World ->
			TypeU.is_subtype ct BuiltinType.world
		| Bool _ | Int _ | Float _ | Void | Rc _ ->
			false
		end

let check(t: ty)(v: v): unit =
	if not (f t v) then
		failwith @@ OutputU.out_to_string "Expected a %a, got %a" TypeU.output t ValU.output v
