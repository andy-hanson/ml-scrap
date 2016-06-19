type t =
	| Any
	| Bool
	| Float
	| Int
	| Void
	| Ft of ft
	| Rc of rc
	| Un of un

and parameter = Sym.t * t
and ft = {
	fname: Sym.t;
	mutable return_type: t;
	mutable parameters: parameter array
}

(*TODO: Sym.t * t*)
and property = {
	prop_name: Sym.t;
	prop_type: t
}
and rc = {
	rname: Sym.t;
	(* Mutable for sake of rec-creating algorithm *)
	mutable properties: property array
}

and un = {
	uname: Sym.t;
	mutable types: t array
}
