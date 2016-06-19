let rec f(t: Type.t)(v: Val.t): bool =
	match t with
	| Type.Any ->
		true
	| Type.Bool ->
		begin match v with
		| Val.Bool _ -> true
		| _ -> false
		end
	| Type.Float ->
		begin match v with
		| Val.Float _ -> true
		| _ -> false
		end
	| Type.Int ->
		begin match v with
		| Val.Int _ -> true
		| _ -> false
		end
	| Type.Void ->
		begin match v with
		| Val.Void -> true
		| _ -> false
		end
	| Type.Ft _ ->
		begin match v with
		| Val.Fn _->
			TypeU.is_subtype t (ValU.type_of v)
		| Val.BuiltinFn {Val.typ; _} ->
			TypeU.is_subtype t (Type.Ft typ)
		| _ ->
			false
		end
	| Type.Rc rc ->
		begin match v with
		| Val.Rc(val_rc, _) -> rc == val_rc
		| _ -> false
		end
	| Type.Un {Type.types; _} ->
		ArrayU.exists types (fun t -> f t v)

let check(t: Type.t)(v: Val.t): unit =
	if not (f t v) then
		failwith (OutputU.out_to_string "Expected a %a, got %a" TypeU.output t ValU.output v)
