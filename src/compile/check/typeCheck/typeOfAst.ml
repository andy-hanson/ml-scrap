open N.V
open N.Ty
open N.TyP
open N.Code
open N.Compiler

(*TODO: rename to ?????????.ml*)

module Rts = AstLookup.Rt
type rts = rt Rts.t
module GenRts = AstLookup.GenRt
type gen_rts = gen_rt GenRts.t
module Uns = AstLookup.Un
type uns = un Uns.t
module Fts = AstLookup.Ft
type fts = ft Fts.t
module Fns = AstLookup.Fn
type fns = declared_fn Fns.t
(*TODO:KILL (just use declared_tys)*)
module ParameterTys = AstLookup.Parameter
type parameter_tys = ty ParameterTys.t
module DeclaredTys = AstLookup.Ty
type declared_tys = ty DeclaredTys.t
type t = {rts: rts; gen_rts: gen_rts; uns: uns; fts: fts; fns: fns; parameter_tys: parameter_tys; declared_tys: declared_tys}

(*TODO: which of these are really needed?*)
let rt_of_ast({rts; _}: t): Ast.rt -> rt = Rts.get rts
let gen_rt_of_ast({gen_rts; _}: t): Ast.gen_rt -> gen_rt = GenRts.get gen_rts
let un_of_ast({uns; _}: t): Ast.un -> un = Uns.get uns
let ft_of_ast({fts; _}: t): Ast.ft -> ft = Fts.get fts
let fn_of_ast({fns; _}: t): Ast.fn -> declared_fn = Fns.get fns

let parameter_ty({parameter_tys; _}: t): Ast.parameter -> ty =
	ParameterTys.get parameter_tys

let set_members(members: modul_member Sym.Lookup.t)({fns; rts; gen_rts; uns; fts; parameter_tys = _; declared_tys = _}: t): unit =
	let set = Sym.Lookup.set members in
	Fns.iter fns begin fun fn_ast fn ->
		set (AstU.fn_name fn_ast) @@ V(Fn(DeclaredFn fn))
	end;
	Rts.iter rts (fun (_, name, _) r -> set name @@ Ty(Rt r));
	GenRts.iter gen_rts (fun (_, name, _, _) r -> set name @@ Ty(GenRt r));
	Uns.iter_values uns (fun u -> set u.uname @@ Ty(Un u));
	Fts.iter fts (fun (_, name, _) f -> set name @@ Ty(Ft f))

let val_of_ast(type_of_ast: t)(ast: Ast.decl_val): v =
	match ast with
	| Ast.Fn f ->
		Fn(DeclaredFn(fn_of_ast type_of_ast f))

let ty_of_ast(type_of_ast: t)(ast: Ast.decl_ty): ty =
	begin match ast with
	| Ast.Rt r ->
		Rt(rt_of_ast type_of_ast r)
	| Ast.GenRt g ->
		GenRt(gen_rt_of_ast type_of_ast g)
	| Ast.Un u ->
		Un(un_of_ast type_of_ast u)
	| Ast.Ft f ->
		Ft(ft_of_ast type_of_ast f)
	end


module Vars = Lookup.Make(struct
	type t = gen_var
	let equal: t -> t -> bool = (==)
	let hash((loc, _): gen_var): int =
		Loc.hash loc
end)
type vars = ty Vars.t

let substitute_vars(vars: vars): ty -> ty = function
	| GenVar v -> Vars.get vars v
	| t -> t

let instantiate_gen_rt(loc: Loc.t)(gen_rt: gen_rt)(tys: ty array): rt =
	(*TODO: we need a check that the rt has been built yet, in case generic types recursively refer to each other.*)
	let { gen_rt_origin = _; gen_rt_params; gen_rt_properties; gen_rt_cache } = gen_rt in
	match N.GenCache.try_get gen_rt_cache tys with
	| Some rt -> rt
	| None ->
		U.returning { rt_origin = RtGenInst(gen_rt, tys); properties = [||] } @@ fun rt ->
			N.GenCache.set gen_rt_cache tys rt;
			ErrU.check (ArrayU.same_length gen_rt_params tys) loc
				(Err.GenInstParameters(Array.length gen_rt_params, Array.length tys));
			let vars = Vars.build_from_keys_and_values gen_rt_params tys in
			rt.properties <- ArrayU.map gen_rt_properties @@ fun (name, ty_with_vars) ->
				name, substitute_vars vars ty_with_vars

(*TODO: share more code with instantiate_gen_rt*)
(*TODO: share more code with instantiate_gen_rt*)
(*TODO: share more code with instantiate_gen_rt*)
let instantiate_gen_ft(loc: Loc.t)(gen_ft: gen_ft)(tys: ty array): ft =
	let { gen_ft_origin = _; gen_ft_ty_params; gen_ft_return; gen_ft_parameters; gen_ft_cache } = gen_ft in
	match N.GenCache.try_get gen_ft_cache tys with
	| Some ft -> ft
	| None ->
		U.returning { ft_origin = FtGenInst(gen_ft, tys); return = t_nil; parameters = [||] } @@ fun ft ->
			N.GenCache.set gen_ft_cache tys ft;
			ErrU.check (ArrayU.same_length gen_ft_ty_params tys) loc
				(Err.GenInstParameters(Array.length gen_ft_ty_params, Array.length tys));
			let vars = Vars.build_from_keys_and_values gen_ft_ty_params tys in
			ft.return <- substitute_vars vars gen_ft_return;
			ft.parameters <- ArrayU.map gen_ft_parameters @@ fun (name, ty) -> name, substitute_vars vars ty


(*TODO:NAME*)
let rec declared_ty(bindings: Bind.t)({declared_tys; _} as type_of_ast: t): Ast.ty -> ty =
	let rec recur(ty_ast: Ast.ty): ty =
		match DeclaredTys.try_get declared_tys ty_ast with
		| Some ty -> ty
		| None ->
			(*TODO: Write a placeholder now, and if we see the placeholder again, error.*)
			let ty =
				begin match ty_ast with
				| Ast.TyAccess(access) ->
					begin match Bind.ty_binding bindings access with
					| Binding.ExternalTy b ->
						b
					| Binding.TDeclared d ->
						ty_of_ast type_of_ast d
					| Binding.TParameter p ->
						GenVar p
					end
				| Ast.TyInst(loc, ty, tys) ->
					instantiate_generic bindings type_of_ast loc (recur ty) tys
				end in
			U.returning ty @@ DeclaredTys.set declared_tys ty_ast in
	recur

and instantiate_generic(bindings: Bind.t)(type_of_ast: t)(loc: Loc.t)(ty: ty)(tys: Ast.ty array): ty =
	let args = ArrayU.map tys (declared_ty bindings type_of_ast) in
	begin match ty with
	| GenRt g ->
		Rt(instantiate_gen_rt loc g args)
	| GenFt g ->
		Ft(instantiate_gen_ft loc g args)
	| _ ->
		U.todo()
	end

let build(path: Path.t)(full_path: Path.t)(bindings: Bind.t)((_, decls): Ast.modul): modul * t =
	let modul = {path; full_path; members = Sym.Lookup.create()} in
	let rts, gen_rts, uns, fts, fns, parameter_tys, declared_tys =
		Rts.create(), GenRts.create(), Uns.create(), Fts.create(), Fns.create(), ParameterTys.create(), DeclaredTys.create() in

	(*TODO: rename type_of_ast to something more appropriate*)
	U.returning (modul, {rts; gen_rts; uns; fts; fns; parameter_tys; declared_tys}) @@ fun (modul, type_of_ast) ->
		ArrayU.iter decls begin function
			| Ast.DeclVal v ->
				begin match v with
				| Ast.Fn((_, head, _, _) as fn_ast) ->
					let dummy_code = {bytecodes = [||]; locs = CodeLocs.empty} in
					let fn =
						match head with
						| Ast.FnPlain _ ->
							let rec fn = {fn_ast; fn_ty; fn_mdl = modul; fn_code = dummy_code}
							and fn_ty = FoG_Ft {ft_origin = FtFromFn fn; return = t_nil; parameters = [||]} in
							fn
						| Ast.FnGeneric(_, params) ->
							let rec fn = {fn_ast; fn_ty; fn_mdl = modul; fn_code = dummy_code}
							and fn_ty =
								FoG_Gen {
									gen_ft_origin = GenFtFromFn fn;
									gen_ft_ty_params = params;
									gen_ft_return = t_nil;
									gen_ft_parameters = [||];
									gen_ft_cache = N.GenCache.create()
								} in
							fn in
					Fns.set fns fn_ast fn
				end
			| Ast.DeclTy t ->
				begin match t with
				| Ast.Rt r ->
					Rts.set rts r {rt_origin = RtDecl r; properties = [||]}
				| Ast.GenRt((_, _, params, _) as g) ->
					GenRts.set gen_rts g {gen_rt_origin = g; gen_rt_params = params; gen_rt_properties = [||]; gen_rt_cache = N.GenCache.create()}
				| Ast.Un((_, uname, _) as u) ->
					Uns.set uns u {uname; utys = [||]}
				| Ast.Ft(ft_ast) ->
					Fts.set fts ft_ast @@ {ft_origin = FtDecl ft_ast; return = t_nil; parameters = [||]}
				end
		end;
		set_members modul.members type_of_ast;

		let declared_ty: Ast.ty -> ty = declared_ty bindings type_of_ast in

		(* Fill in types *)
		Rts.iter rts begin fun (_, _, properties) rt ->
			rt.properties <- ArrayU.map properties @@ fun (_, name, ty) -> name, declared_ty ty
		end;
		GenRts.iter gen_rts begin fun (_, _, _, properties) g ->
			g.gen_rt_properties <- ArrayU.map properties @@ fun (_, name, ty) -> name, declared_ty ty
		end;
		Uns.iter uns begin fun (_, _, tys) un ->
			un.utys <- ArrayU.map tys declared_ty
		end;

		let fill_in_parameters(parameters: Ast.parameter array): parameter array =
			ArrayU.map parameters @@ fun ((_, name, ty) as param) ->
				name, U.returning (declared_ty ty) @@ ParameterTys.set parameter_tys param in

		let fill_in_ft((_, return, parameters): Ast.signature)(ft: ft): unit =
			ft.return <- declared_ty return;
			ft.parameters <- fill_in_parameters parameters in

		let fill_in_gen_ft((_, return, parameters): Ast.signature)(g: gen_ft): unit =
			g.gen_ft_return <- declared_ty return;
			g.gen_ft_parameters <- fill_in_parameters parameters in

		Fts.iter fts (fun (_, _, signature) -> fill_in_ft signature);

		(* Fill in types of values *)
		Fns.iter fns begin fun (_, _, signature, _) {fn_ty; _} ->
			match fn_ty with
			| FoG_Ft ft -> fill_in_ft signature ft
			| FoG_Gen g -> fill_in_gen_ft signature g
		end

(*TODO:output everything!*)
let output(_out: 'o OutputU.t)({fns = _; _}: t): unit =
	U.todo()
	(*let output_fn(out)((_, name, _, _)) = Sym.output out name in
	(Fns.output output_fn ValOut.output_declared_fn) out fns*)
