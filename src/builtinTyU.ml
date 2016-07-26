open N.Ty

let ft(name: Sym.t)(return: ty)(parameters: parameter array): ft =
	{ft_origin = FtBuiltin name; return; parameters}

let t_ft(name: Sym.t)(return: ty)(parameters: parameter array): ty =
	Ft(ft name return parameters)

let t_rc(name: Sym.t)(properties: property array): ty =
	Rt {rt_origin = RtBuiltin name; properties}

let ft_n(name: string)(return_ty: ty)(parameters: (string * ty) array): ty =
	t_ft (Sym.of_string name) return_ty @@ ArrayU.map parameters @@ fun (name, ty) -> Sym.of_string name, ty

let ft0(name: string)(return_ty: ty): ty =
	ft_n name return_ty [| |]

let ft1(name: string)(return_ty: ty)(param0_name: string)(param0_ty: ty): ty =
	ft_n name return_ty [| param0_name, param0_ty |]


let rc_n(name: string)(props: (string * ty) array): ty =
	t_rc (Sym.of_string name) @@ ArrayU.map props @@ fun (name, ty) -> Sym.of_string name, ty

let rc1(name: string)(prop_name: string)(ty: ty): ty =
	rc_n name [| prop_name, ty |]
