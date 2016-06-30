(*TODO: rename to ?????????.ml*)

module Rts = AstU.RtLookup
type rts = N.rt Rts.t
module Uns = AstU.UnLookup
type uns = N.un Uns.t
module Fts = AstU.FtLookup
type fts = N.ft Fts.t
module Cts = AstU.CtLookup
type cts = N.ct Cts.t
module Fns = AstU.FnLookup
type fns = N.declared_fn Fns.t
module ParameterTypes = AstU.ParameterLookup
type parameter_types = N.ty ParameterTypes.t
module Cns = AstU.CnLookup
type cns = N.declared_fn Cns.t
type t = {rts: rts; uns: uns; fts: fts; cts: cts; fns: fns; parameter_types: parameter_types; cns: cns}

(*TODO: which of these are really needed?*)
let rt_of_ast({rts; _}: t): Ast.rt -> N.rt = Rts.get rts
let un_of_ast({uns; _}: t): Ast.un -> N.un = Uns.get uns
let ft_of_ast({fts; _}: t): Ast.ft -> N.ft = Fts.get fts
let ct_of_ast({cts; _}: t): Ast.ct -> N.ct = Cts.get cts
let fn_of_ast({fns; _}: t): Ast.fn -> N.declared_fn = Fns.get fns
let parameter_type({parameter_types; _}: t): Ast.parameter -> N.ty = ParameterTypes.get parameter_types
let cn_of_ast({cns; _}: t): Ast.cn -> N.declared_fn = Cns.get cns

let set_type_by_name(type_by_name: N.ty Sym.Lookup.t)({rts; uns; fts; cts; fns = _; parameter_types = _; cns = _}: t): unit =
	let set = Sym.Lookup.set type_by_name in
	Rts.iter_values rts (fun ({N.rname; _} as r) -> set rname @@ N.Rt r);
	Uns.iter_values uns (fun ({N.uname; _} as u) -> set uname @@ N.Un u);
	Fts.iter_values fts (fun ({N.fname; _} as f) -> set fname @@ N.TFn(N.Ft f));
	Cts.iter_values cts (fun ({N.cname; _} as c) -> set cname @@ N.TFn(N.Ct c))

let set_value_by_name(value_by_name: N.v Sym.Lookup.t)({fns; cns; rts = _; uns = _; fts = _; cts = _; parameter_types = _}: t): unit =
	let set = Sym.Lookup.set value_by_name in
	Fns.iter_values fns (fun fn -> set (ValU.fn_name fn) @@ N.Fn(N.DeclaredFn fn));
	Cns.iter_values cns (fun cn -> set (ValU.fn_name cn) @@ N.Fn(N.DeclaredFn cn))

let type_of_ast(type_of_ast: t)(ast: Ast.decl): N.ty =
	begin match ast with
	| Ast.Fn _ | Ast.Cn _ ->
		assert false
	| Ast.Rt r ->
		N.Rt(rt_of_ast type_of_ast r)
	| Ast.Un u ->
		N.Un(un_of_ast type_of_ast u)
	| Ast.Ft f ->
		N.TFn(N.Ft(ft_of_ast type_of_ast f))
	| Ast.Ct c ->
		N.TFn(N.Ct(ct_of_ast type_of_ast c))
	end
(*TODO: kill above? or at least write it in terms of this*)
let ty_or_v_of_ast(type_of_ast: t)(ast: Ast.decl): N.ty_or_v =
	begin match ast with
	| Ast.Fn f ->
		N.V(N.Fn(N.DeclaredFn(fn_of_ast type_of_ast f)))
	| Ast.Cn c ->
		N.V(N.Fn(N.DeclaredFn(cn_of_ast type_of_ast c)))
	| Ast.Rt r ->
		N.Ty(N.Rt(rt_of_ast type_of_ast r))
	| Ast.Un u ->
		N.Ty(N.Un(un_of_ast type_of_ast u))
	| Ast.Ft f ->
		N.Ty(N.TFn(N.Ft(ft_of_ast type_of_ast f)))
	| Ast.Ct c ->
		N.Ty(N.TFn(N.Ct(ct_of_ast type_of_ast c)))
	end

let declared_type(binding: Ast.access -> Binding.t)(t: t)(typ: Ast.typ): N.ty =
	match typ with
	| Ast.TypeAccess(access) ->
		begin match binding access with
		| Binding.Builtin _ | Binding.Local _ | Binding.Parameter _ ->
			raise U.TODO (*TODO not-a-type error*)
		| Binding.BuiltinType b ->
			b
		| Binding.Declared d ->
			type_of_ast t d
		end

let dummy_ft(fname: Sym.t): N.ft =
	{N.fname; return_type = N.t_void; parameters = [||]}
let dummy_ct(cname: Sym.t): N.ct =
	{N.cname; N.ct_cases = [||]}
let dummy_code: N.code =
	{N.bytecodes = [||]; N.locs = CodeLocs.empty}

let build(path: FileIO.path)(binding: Ast.access -> Binding.t)(decls: Ast.modul): N.modul * t =
	let modul = {N.path; N.values = Sym.Lookup.create(); N.types = Sym.Lookup.create()} in
	let rts, uns, fts, cts, fns, parameter_types, cns =
		Rts.create(), Uns.create(), Fts.create(), Cts.create(), Fns.create(), ParameterTypes.create(), Cns.create() in

	(*TODO: rename type_of_ast to something more appropriate*)
	U.returning (modul, {rts; uns; fts; cts; fns; parameter_types; cns}) begin fun (modul, type_of_ast) ->
		ArrayU.iter decls begin function
			| Ast.Rt((_, rname, _) as r) -> Rts.set rts r {N.rname; N.properties = [||]}
			| Ast.Un((_, uname, _) as u) -> Uns.set uns u {N.uname; N.utypes = [||]}
			| Ast.Ft((_, fname, _) as f) -> Fts.set fts f {N.fname; return_type = N.t_void; parameters = [||]}
			| Ast.Ct((_, cname, _) as c) -> Cts.set cts c {N.cname; ct_cases = [||]}
			| Ast.Fn((_, name, _, _) as fn) ->
				Fns.set fns fn {
					N.fn_type = N.Ft(dummy_ft name);
					N.containing_modul = modul;
					N.code = dummy_code
				}
			| Ast.Cn((_, name, _, _) as cn) ->
				(*TODO: this is very similar to above. Share code.*)
				Cns.set cns cn {
					N.fn_type = N.Ct(dummy_ct name);
					N.containing_modul = modul;
					N.code = dummy_code
				}
		end;
		set_type_by_name modul.N.types type_of_ast;
		set_value_by_name modul.N.values type_of_ast;

		let declared_type = declared_type binding type_of_ast in

		(* Fill in types *)
		Rts.iter rts begin fun (_, _, properties) rt ->
			rt.N.properties <- ArrayU.map properties @@ fun (_, name, typ) -> name, declared_type typ
		end;
		Uns.iter uns begin fun (_, _, types) un ->
			un.N.utypes <- ArrayU.map types declared_type
		end;
		let fill_in_ft((_, return_type, parameters): Ast.signature)(ft: N.ft): unit =
			ft.N.return_type <- declared_type return_type;
			ft.N.parameters <- ArrayU.map parameters begin fun ((_, name, type_ast) as param) ->
				name, U.returning (declared_type type_ast) @@ ParameterTypes.set parameter_types param
			end in
		Fts.iter fts (fun (_, _, signature) -> fill_in_ft signature);
		Cts.iter cts begin fun (_, _, case_asts) ct ->
			ct.N.ct_cases <- ArrayU.map case_asts @@ fun (return, input) -> declared_type return, declared_type input
		end;

		(* Fill in types of values *)
		Fns.iter fns begin fun (_, _, signature, _) {N.fn_type; _} ->
			let ft = match fn_type with | N.Ft ft -> ft | _ -> assert false in
			fill_in_ft signature ft
		end;
		Cns.iter cns begin fun (_, _, typ, _) {N.fn_type; _} ->
			let ct = match fn_type with | N.Ct ct -> ct | _ -> assert false in
			ct.N.ct_cases <- match declared_type typ with
				| N.TFn(N.Ct ct) -> ct.N.ct_cases
				| _ -> assert false (*TODO: appropriate error*)
		end
	end

(*TODO:output everything!*)
let output(out: 'o OutputU.t)({fns; _}: t): unit =
	let output_fn(out)((_, name, _, _)) = Sym.output out name in
	(Fns.output output_fn ValU.output_declared_fn) out fns
