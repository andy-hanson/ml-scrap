open N.Ty

let rec output_rt_origin(out: 'o OutputU.t): rt_origin -> unit = function
	| RtBuiltin name ->
		Sym.output out name
	| RtDecl(_, name, _) ->
		Sym.output out name
	| RtGenInst(g, inst_with) ->
		OutputU.out out "[%a %a]"
			Sym.output (TyU.name (GenRt g))
			(ArrayU.output_elements ~delimeter:" " output_brief) inst_with

and output_rt(out: 'o OutputU.t)({rt_id; rt_origin; properties}: rt): unit =
	let output_property(out: 'o OutputU.t)((name, ty): property): unit =
		OutputU.out out "%a: %a"
			Sym.output name
			output ty in
	OutputU.out out "Rt(%i, %a, %a)"
		rt_id
		output_rt_origin rt_origin
		(ArrayU.output output_property) properties

and output_un_origin(out: 'o OutputU.t): un_origin -> unit = function
	| UnBuiltin name ->
		Sym.output out name
	| UnDecl(_, name, _) ->
		Sym.output out name
	| UnGenInst(_g, _inst_with) ->
		U.todo()

and output_un(out: 'o OutputU.t)({un_origin; utys}: un): unit =
	OutputU.out out "Un(%a, %a)"
		output_un_origin un_origin
		(ArrayU.output output) utys

and output_ft_origin(out: 'o OutputU.t): ft_origin -> unit = function
	| FtBuiltin name -> Sym.output out name
	| FtDecl(_, name, _) -> Sym.output out name
	| FtFromFn _ -> U.todo()
	| FtFromRt _ -> U.todo()
	| FtFromPartial _ -> U.todo()
	| FtGenInst _ -> U.todo()

and output_ft(out: 'o OutputU.t)({ft_origin = _; return; parameters}: ft): unit =
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

and output_brief(out: 'o OutputU.t): ty -> unit = function
	| TPrimitive p -> Sym.output out @@ TyU.primitive_name p
	| Rt rt ->
		(*TODO: probably don't need id here*)
		OutputU.out out "%i@%a"
			rt.rt_id
			output_rt_origin rt.rt_origin
	| Un un -> output_un_origin out un.un_origin
	| Ft ft -> output_ft_origin out ft.ft_origin
	| GenRt g ->
		let (_, name, _, _) = g.gen_rt_origin in
		Sym.output out name
	| GenUn _ -> U.todo()
	| GenFt _ -> U.todo()
	| GenVar(_, name) -> Sym.output out name

and output(out: 'o OutputU.t)(t: ty): unit =
	match t with
	| TPrimitive _ ->
		output_brief out t
	| Rt rt ->
		output_rt out rt
	| Un un ->
		output_un out un
	| Ft ft ->
		output_ft out ft
	| GenRt _ ->
		U.todo()
	| GenUn _ ->
		U.todo()
	| GenFt _ ->
		U.todo()
	| GenVar(loc, name) ->
		OutputU.out out "GenVar(%a@%a)"
			Sym.output name
			Loc.output loc
