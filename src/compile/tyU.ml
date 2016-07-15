open N

let name = function
	| Any -> Sym.of_string "Any"
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
	| TFn f ->
		begin match f with
		| Ft {fname; _} -> fname
		| Ct {cname; _} -> cname
		end

let ft(fname: Sym.t)(return: ty)(parameters: parameter array): ft =
	{fname; return; parameters}

let t_ft(fname: Sym.t)(return: ty)(parameters: parameter array): ty =
	TFn(Ft(ft fname return parameters))

let t_rc(rname: Sym.t)(properties: property array): ty =
	Rt {rname; properties}

let ft_arity({parameters; _}: ft): int =
	Array.length parameters
let rt_arity({properties; _}: rt): int =
	Array.length properties
(*TODO: input type should just be ty_fn*)
let arity: ty -> int = function
	| TFn f ->
		begin match f with
		| Ft f -> ft_arity f
		| Ct _ -> 1
		end
	| _ -> assert false

let partial_ty(fn_ty: ty_fn)(args: ty array): ft =
	match fn_ty with
	| Ft {fname; return; parameters} ->
		let parameters = Array.sub parameters 0 (Array.length parameters - Array.length args) in
		{fname; return; parameters}
	| Ct _ ->
		raise U.TODO

let ct_input({cname; ct_cases}: ct): un =
	{
		uname = cname; (*TODO: should say "input type for {cname}"*)
		utys = ArrayU.map ct_cases snd
	}

let ct_output({cname; ct_cases}: ct): un =
	{
		uname = cname; (*TODO: should say "output type for {cname}"*)
		utys = ArrayU.map ct_cases fst
	}

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
		(OutputU.out_array output_parameter) parameters

and output_ct(_out: 'o OutputU.t)({cname = _; ct_cases = _}: ct): unit =
	raise U.TODO

and output_ty_fn(out: 'o OutputU.t)(fn_ty: ty_fn): unit =
	match fn_ty with
	| Ft ft -> output_ft out ft
	| Ct ct -> output_ct out ct

and output_rt(out: 'o OutputU.t)({rname; properties}: rt): unit =
	OutputU.out out "Record(%a, %a)" Sym.output rname (OutputU.out_array output_property) properties

and output_short(out: 'o OutputU.t)(t: ty): unit =
	OutputU.str out @@ Sym.string_of @@ name t

and output_brief(out: 'o OutputU.t)(t: ty): unit =
	OutputU.str out @@ Sym.string_of @@ name t

and output(out: 'o OutputU.t)(t: ty): unit =
	match t with
	| Any | TPrimitive _ ->
		output_brief out t
	| Rt rt ->
		output_rt out rt
	| Un {uname; utys} ->
		OutputU.out out "Un(%a, %a)"
			Sym.output uname
			(OutputU.out_array output) utys
	| TFn f ->
		begin match f with
		| Ft ft ->
			output_ft out ft
		| Ct {cname; ct_cases} ->
			let out_case(out: 'o OutputU.t)((return, input): ty * ty): unit =
				OutputU.out out "%a %a" output_short return output_short input in
			OutputU.out out "Ct(%a, %a)"
				Sym.output cname
				(OutputU.out_array out_case) ct_cases
		end
