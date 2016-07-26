open N.Ty

let rec output_ft(out: 'o OutputU.t)({ft_origin = _; return; parameters}: ft): unit =
	U.todo(); (*TODO: use origin *)
	let output_parameter(out: 'o OutputU.t)((name, ty): parameter): unit =
		OutputU.out out "%a %a"
			Sym.output name
			output ty in
	OutputU.out out "Fn(%a, %a)"
		output return
		(ArrayU.output output_parameter) parameters

and output_gen_ft(_out: 'o OutputU.t)(_: gen_ft): unit =
	U.todo()

and output_ft_or_gen(out: 'o OutputU.t): ft_or_gen -> unit = function
	| FoG_Ft f -> output_ft out f
	| FoG_Gen g -> output_gen_ft out g

and output_rt_origin(out: 'o OutputU.t): rt_origin -> unit = function
	| RtBuiltin name ->
		Sym.output out name
	| RtDecl(_, name, _) ->
		Sym.output out name
	| RtGenInst(g, inst_with) ->
		OutputU.out out "[%a %a]"
			Sym.output (TyU.name (GenRt g))
			(ArrayU.output_elements ~delimeter:" " output_brief) inst_with

and output_rt(out: 'o OutputU.t)({rt_origin; properties}: rt): unit =
	let output_property(out: 'o OutputU.t)((name, ty): property): unit =
		OutputU.out out "Property(%a, %a)"
			Sym.output name
			output ty in
	OutputU.out out "Record(%a, %a)"
		output_rt_origin rt_origin
		(ArrayU.output output_property) properties

and output_brief(out: 'o OutputU.t)(t: ty): unit =
	OutputU.str out @@ Sym.string_of @@ TyU.name t

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
	| GenRt _ ->
		U.todo()
	| GenFt _ ->
		U.todo()
	| GenVar(loc, name) ->
		OutputU.out out "GenVar(%a@%a)"
			Sym.output name
			Loc.output loc
