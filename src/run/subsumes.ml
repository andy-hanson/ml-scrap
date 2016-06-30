open N

let rec f(t: ty)(v: v): bool =
	match t with
	| Any ->
		true
	| TPrimitive tp ->
		begin match v with
		| Primitive p -> tp = ValU.type_of_primitive p
		| _ -> false
		end
	| Rt rc ->
		begin match v with
		| Rc(val_rc, _) -> rc == val_rc
		| _ -> false
		end
	| Un {utypes; _} ->
		ArrayU.exists utypes @@ fun t -> f t v
	| TFn f ->
		begin match f with
		| Ft _ ->
			begin match v with
			| Fn _ ->
				(*TODO: get type, and check subtyping*)
				raise U.TODO
			| Primitive _ | Rc _ ->
				false
			end
		| Ct {ct_cases = _; _} as _ct ->
			(*TODO: v must be a Fn or BuiltinFn whose type is_subtype, much like the Ft case (so share code!)*)
			begin match v with
			| Fn _ ->
				raise U.TODO
			| Primitive _ | Rc _ ->
				false
			end
		end

let check(t: ty)(v: v): unit =
	if not (f t v) then
		failwith @@ OutputU.out_to_string "Expected a %a, got %a" TypeU.output t ValU.output v
