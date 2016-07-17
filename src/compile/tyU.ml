open N

let name = function
	| TPrimitive p ->
		Sym.of_string begin match p with
		| TBool -> "Bool"
		| TFloat -> "Float"
		| TInt -> "Int"
		| TString -> "String"
		| TVoid -> "Void"
		end
	| Rt {rname; _} -> rname
	| Un {uname; _} -> uname
	| Ft {fname; _} -> fname
	| TyGen _ | TyVar _ | TyInst _ -> raise U.TODO

let ft(fname: Sym.t)(return: ty)(parameters: parameter array): ft =
	{fname; return; parameters}

let t_ft(fname: Sym.t)(return: ty)(parameters: parameter array): ty =
	Ft(ft fname return parameters)

let t_rc(rname: Sym.t)(properties: property array): ty =
	Rt {rname; properties}

let ft_arity({parameters; _}: ft): int =
	Array.length parameters
let rt_arity({properties; _}: rt): int =
	Array.length properties
(*TODO: input type should just be ty_fn*)
let arity: ty -> int = function
	| Ft f -> ft_arity f
	| _ -> assert false

let partial_ty({fname; return; parameters}: ft)(args: ty array): ft =
	let parameters = Array.sub parameters 0 (Array.length parameters - Array.length args) in
	{fname; return; parameters}

let rec output_property(out: 'o OutputU.t)((name, ty): property): unit =
	OutputU.out out "Property(%a, %a)"
		Sym.output name
		output ty

and output_ft(out: 'o OutputU.t)({fname; return; parameters}: ft): unit =
	let output_parameter(out: 'o OutputU.t)((name, ty): parameter): unit =
		OutputU.out out "%a %a"
			Sym.output name
			output ty in
	OutputU.out out "Fn(%a, %a, %a)"
		Sym.output fname
		output return
		(ArrayU.output output_parameter) parameters

and output_rt(out: 'o OutputU.t)({rname; properties}: rt): unit =
	OutputU.out out "Record(%a, %a)" Sym.output rname (ArrayU.output output_property) properties

and output_brief(out: 'o OutputU.t)(t: ty): unit =
	OutputU.str out @@ Sym.string_of @@ name t

and output(out: 'o OutputU.t)(t: ty): unit =
	match t with
	| TPrimitive _ ->
		output_brief out t
	| Rt rt ->
		output_rt out rt
	| Un {uname; utys} ->
		OutputU.out out "Un(%a, %a)"
			Sym.output uname
			(ArrayU.output output) utys
	| Ft ft ->
		output_ft out ft
	| TyGen _ | TyVar _ | TyInst _ ->
		raise U.TODO
