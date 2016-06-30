open N

let rc_n(name: string)(props: (string * ty) array): ty =
	TypeU.t_rc (Sym.of_string name) @@ ArrayU.map props @@ fun (name, typ) -> Sym.of_string name, typ

let rc1(name: string)(prop_name: string)(typ: ty): ty =
	rc_n name [| prop_name, typ |]
