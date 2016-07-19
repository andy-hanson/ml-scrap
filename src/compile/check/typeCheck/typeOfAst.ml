open N

(*TODO: rename to ?????????.ml*)

module Rts = AstU.RtLookup
type rts = rt Rts.t
module Uns = AstU.UnLookup
type uns = un Uns.t
module Fts = AstU.FtLookup
type fts = ft Fts.t
module Fns = AstU.FnLookup
type fns = declared_fn Fns.t
module ParameterTys = AstU.ParameterLookup
type parameter_tys = ty ParameterTys.t
type t = {rts: rts; uns: uns; fts: fts; fns: fns; parameter_tys: parameter_tys}

(*TODO: which of these are really needed?*)
let rt_of_ast({rts; _}: t): Ast.rt -> rt = Rts.get rts
let un_of_ast({uns; _}: t): Ast.un -> un = Uns.get uns
let ft_of_ast({fts; _}: t): Ast.ft -> ft = Fts.get fts
let fn_of_ast({fns; _}: t): Ast.fn -> declared_fn = Fns.get fns
(*TODO: kill this function?*)
let ft_of_fn(t: t)(fn: Ast.fn): ft =
	let {fn_ty; _} = fn_of_ast t fn in
	fn_ty

let parameter_ty({parameter_tys; _}: t): Ast.parameter -> ty =
	ParameterTys.get parameter_tys

let set_type_by_name(type_by_name: ty Sym.Lookup.t)({rts; uns; fts; fns = _; parameter_tys = _}: t): unit =
	let set = Sym.Lookup.set type_by_name in
	Rts.iter_values rts (fun ({rname; _} as r) -> set rname @@ Rt r);
	Uns.iter_values uns (fun ({uname; _} as u) -> set uname @@ Un u);
	Fts.iter_values fts (fun ({fname; _} as f) -> set fname @@ Ft f)

let set_value_by_name(value_by_name: v Sym.Lookup.t)({fns; rts = _; uns = _; fts = _; parameter_tys = _}: t): unit =
	let set = Sym.Lookup.set value_by_name in
	Fns.iter_values fns (fun fn -> set (ValU.fn_name fn) @@ Fn(DeclaredFn fn))

let val_of_ast(type_of_ast: t)(ast: Ast.decl_val): v =
	let fn =
		match ast with
		| Ast.Fn f ->
			fn_of_ast type_of_ast f in
	Fn(DeclaredFn fn)

let ty_of_ast(type_of_ast: t)(ast: Ast.decl_ty): ty =
	begin match ast with
	| Ast.Rt r ->
		Rt(rt_of_ast type_of_ast r)
	| Ast.Un u ->
		Un(un_of_ast type_of_ast u)
	| Ast.Ft f ->
		Ft(ft_of_ast type_of_ast f)
	end

let declared_ty(bindings: Bind.t)(t: t)(typ: Ast.ty): ty =
	match typ with
	| Ast.TyAccess(access) ->
		begin match Bind.ty_binding bindings access with
		| Binding.ExternalTy b ->
			b
		| Binding.TDeclared d ->
			ty_of_ast t d
		end
	| _ ->
		raise U.TODO

let dummy_ft(fname: Sym.t): ft =
	{fname; return = t_void; parameters = [||]}
let dummy_code: code =
	{bytecodes = [||]; locs = CodeLocs.empty}

let build(path: Path.t)(full_path: Path.t)(bindings: Bind.t)((_, decls): Ast.modul): modul * t =
	let modul = {path; full_path; vals = Sym.Lookup.create(); tys = Sym.Lookup.create()} in
	let rts, uns, fts, fns, parameter_tys =
		Rts.create(), Uns.create(), Fts.create(), Fns.create(), ParameterTys.create() in

	(*TODO: rename type_of_ast to something more appropriate*)
	U.returning (modul, {rts; uns; fts; fns; parameter_tys}) @@ fun (modul, type_of_ast) ->
		ArrayU.iter decls begin function
			| Ast.DeclVal v ->
				begin match v with
				| Ast.Fn((_, name, _, _) as fn) ->
					Fns.set fns fn @@ {fn_ty = dummy_ft name; fn_mdl = modul; fn_code = dummy_code}
				end
			| Ast.DeclTy t ->
				begin match t with
				| Ast.Rt((_, rname, _) as r) ->
					Rts.set rts r {rname; properties = [||]}
				| Ast.Un((_, uname, _) as u) ->
					Uns.set uns u {uname; utys = [||]}
				| Ast.Ft((_, fname, _) as f) ->
					let dummy(fname: Sym.t): ft =
						{fname; return = t_void; parameters = [||]} in
					let fname =
						match fname with
						| Ast.Plain f -> f
						| Ast.Generic(_, _) -> raise U.TODO in
					Fts.set fts f @@ dummy fname
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
			ft.parameters <- ArrayU.map parameters @@ fun ((_, name, ty) as param) ->
				name, U.returning (declared_ty ty) @@ ParameterTys.set parameter_tys param in
		Fts.iter fts (fun (_, _, signature) -> fill_in_ft signature);

		(* Fill in types of values *)
		Fns.iter fns begin fun (_, _, signature, _) {fn_ty; _} ->
			fill_in_ft signature fn_ty
		end

(*TODO:output everything!*)
let output(out: 'o OutputU.t)({fns; _}: t): unit =
	let output_fn(out)((_, name, _, _)) = Sym.output out name in
	(Fns.output output_fn ValU.output_declared_fn) out fns
