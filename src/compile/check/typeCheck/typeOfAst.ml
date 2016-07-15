open N

(*TODO: rename to ?????????.ml*)

module Rts = AstU.RtLookup
type rts = rt Rts.t
module Uns = AstU.UnLookup
type uns = un Uns.t
module Fts = AstU.FtLookup
type fts = ft Fts.t
module Cts = AstU.CtLookup
type cts = ct Cts.t
module Fns = AstU.FnLookup
type fns = declared_fn Fns.t
module ParameterTys = AstU.ParameterLookup
type parameter_tys = ty ParameterTys.t
module Cns = AstU.CnLookup
type cns = declared_fn Cns.t
type t = {rts: rts; uns: uns; fts: fts; cts: cts; fns: fns; parameter_tys: parameter_tys; cns: cns}

(*TODO: which of these are really needed?*)
let rt_of_ast({rts; _}: t): Ast.rt -> rt = Rts.get rts
let un_of_ast({uns; _}: t): Ast.un -> un = Uns.get uns
let ft_of_ast({fts; _}: t): Ast.ft -> ft = Fts.get fts
let ct_of_ast({cts; _}: t): Ast.ct -> ct = Cts.get cts
let fn_of_ast({fns; _}: t): Ast.fn -> declared_fn = Fns.get fns
let ft_of_fn(t: t)(fn: Ast.fn): ft =
	let {fn_ty; _} = fn_of_ast t fn in
	match fn_ty with
	| Ft ft -> ft
	| Ct _ -> assert false

let parameter_ty({parameter_tys; _}: t): Ast.parameter -> ty =
	ParameterTys.get parameter_tys
(*TODO: returns a declared_fn just like fn_of_ast, rename?*)
let cn_of_ast({cns; _}: t): Ast.cn -> declared_fn =
	Cns.get cns

let set_type_by_name(type_by_name: ty Sym.Lookup.t)({rts; uns; fts; cts; fns = _; parameter_tys = _; cns = _}: t): unit =
	let set = Sym.Lookup.set type_by_name in
	Rts.iter_values rts (fun ({rname; _} as r) -> set rname @@ Rt r);
	Uns.iter_values uns (fun ({uname; _} as u) -> set uname @@ Un u);
	Fts.iter_values fts (fun ({fname; _} as f) -> set fname @@ TFn(Ft f));
	Cts.iter_values cts (fun ({cname; _} as c) -> set cname @@ TFn(Ct c))

let set_value_by_name(value_by_name: v Sym.Lookup.t)({fns; cns; rts = _; uns = _; fts = _; cts = _; parameter_tys = _}: t): unit =
	let set = Sym.Lookup.set value_by_name in
	Fns.iter_values fns (fun fn -> set (ValU.fn_name fn) @@ Fn(DeclaredFn fn));
	Cns.iter_values cns (fun cn -> set (ValU.fn_name cn) @@ Fn(DeclaredFn cn))

let val_of_ast(type_of_ast: t)(ast: Ast.decl_val): v =
	let fn =
		match ast with
		| Ast.Fn f ->
			fn_of_ast type_of_ast f
		| Ast.Cn c ->
			cn_of_ast type_of_ast c in
	Fn(DeclaredFn fn)

let ty_of_ast(type_of_ast: t)(ast: Ast.decl_ty): ty =
	begin match ast with
	| Ast.Rt r ->
		Rt(rt_of_ast type_of_ast r)
	| Ast.Un u ->
		Un(un_of_ast type_of_ast u)
	| Ast.Ft f ->
		TFn(Ft(ft_of_ast type_of_ast f))
	| Ast.Ct c ->
		TFn(Ct(ct_of_ast type_of_ast c))
	end


(*TODO: kill above? or at least write it in terms of this*)
(*TODO:KILL let ty_or_v_of_ast(type_of_ast: t)(ast: Ast.decl): ty_or_v =
	begin match ast with
	| Ast.Rt r ->
		Ty(Rt(rt_of_ast type_of_ast r))
	| Ast.Un u ->
		Ty(Un(un_of_ast type_of_ast u))
	| Ast.Ft f ->
		Ty(TFn(Ft(ft_of_ast type_of_ast f)))
	| Ast.Ct c ->
		Ty(TFn(Ct(ct_of_ast type_of_ast c)))
	end*)

let declared_ty(bindings: Bind.t)(t: t)(typ: Ast.ty): ty =
	match typ with
	| Ast.TypeAccess(access) ->
		begin match Bind.ty_binding bindings access with
		| Binding.BuiltinType b ->
			b
		| Binding.TDeclared d ->
			ty_of_ast t d
		end

let dummy_ft(fname: Sym.t): ft =
	{fname; return = t_void; parameters = [||]}
let dummy_ct(cname: Sym.t): ct =
	{cname; ct_cases = [||]}
let dummy_code: code =
	{bytecodes = [||]; locs = CodeLocs.empty}

let build(path: FileIO.path)(bindings: Bind.t)((_, decls): Ast.modul): modul * t =
	let modul = {path; vals = Sym.Lookup.create(); tys = Sym.Lookup.create()} in
	let rts, uns, fts, cts, fns, parameter_tys, cns =
		Rts.create(), Uns.create(), Fts.create(), Cts.create(), Fns.create(), ParameterTys.create(), Cns.create() in

	(*TODO: rename type_of_ast to something more appropriate*)
	U.returning (modul, {rts; uns; fts; cts; fns; parameter_tys; cns}) begin fun (modul, type_of_ast) ->
		let declared_fn(fn_ty: ty_fn): declared_fn = { fn_ty; fn_mdl = modul; fn_code = dummy_code } in
		ArrayU.iter decls begin function
			| Ast.DeclVal v ->
				begin match v with
				| Ast.Fn((_, name, _, _) as fn) ->
					Fns.set fns fn @@ declared_fn @@ Ft(dummy_ft name)
				| Ast.Cn((_, name, _, _) as cn) ->
					Cns.set cns cn @@ declared_fn @@ Ct(dummy_ct name)
				end
			| Ast.DeclTy t ->
				begin match t with
				| Ast.Rt((_, rname, _) as r) ->
					Rts.set rts r {rname; properties = [||]}
				| Ast.Un((_, uname, _) as u) ->
					Uns.set uns u {uname; utys = [||]}
				| Ast.Ft((_, fname, _) as f) ->
					Fts.set fts f {fname; return = t_void; parameters = [||]}
				| Ast.Ct((_, cname, _) as c) ->
					Cts.set cts c {cname; ct_cases = [||]}
				end
		end;
		set_type_by_name modul.tys type_of_ast;
		set_value_by_name modul.vals type_of_ast;

		let declared_ty = declared_ty bindings type_of_ast in

		(* Fill in types *)
		Rts.iter rts begin fun (_, _, properties) rt ->
			rt.properties <- ArrayU.map properties @@ fun (_, name, ty) -> name, declared_ty ty
		end;
		Uns.iter uns begin fun (_, _, tys) un ->
			un.utys <- ArrayU.map tys declared_ty
		end;
		let fill_in_ft((_, return, parameters): Ast.signature)(ft: ft): unit =
			ft.return <- declared_ty return;
			ft.parameters <- ArrayU.map parameters begin fun ((_, name, ty) as param) ->
				name, U.returning (declared_ty ty) @@ ParameterTys.set parameter_tys param
			end in
		Fts.iter fts (fun (_, _, signature) -> fill_in_ft signature);
		Cts.iter cts begin fun (_, _, case_asts) ct ->
			ct.ct_cases <- ArrayU.map case_asts @@ fun (return, input) -> declared_ty return, declared_ty input
		end;

		(* Fill in types of values *)
		Fns.iter fns begin fun (_, _, signature, _) {fn_ty; _} ->
			let ft = match fn_ty with | Ft ft -> ft | _ -> assert false in
			fill_in_ft signature ft
		end;
		Cns.iter cns begin fun (_, _, typ, _) {fn_ty; _} ->
			let ct = match fn_ty with | Ct ct -> ct | _ -> assert false in
			ct.ct_cases <- match declared_ty typ with
				| TFn(Ct ct) -> ct.ct_cases
				| _ -> assert false (*TODO: appropriate error*)
		end
	end

(*TODO:output everything!*)
let output(out: 'o OutputU.t)({fns; _}: t): unit =
	let output_fn(out)((_, name, _, _)) = Sym.output out name in
	(Fns.output output_fn ValU.output_declared_fn) out fns
