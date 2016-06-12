type t =
	| Bool
	| Float
	| Int
	| Void
	| Or of t array
	| Fn of fn
	| Rc of rc

and property = {
	prop_name: Symbol.t;
	prop_type: t
}

and fn = {
	return_type: t;
	parameters: t array
}

and rc = {
	rname: Symbol.t;
	(* Mutable for sake of rec-creating algorithm *)
	mutable properties: property array
}

let builtins = [| Bool; Float; Int; Void |]
