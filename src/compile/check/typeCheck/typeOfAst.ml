open N.V
open N.Ty
open N.TyP
open N.Code
open N.Compiler

(*TODO: rename to ?????????.ml*)

(*TODO: neater*)
module Rts = AstLookup.Rt
type rts = rt Rts.t
module GenRts = AstLookup.GenRt
type gen_rts = gen_rt GenRts.t
module Uns = AstLookup.Un
type uns = un Uns.t
module GenUns = AstLookup.GenUn
type gen_uns = gen_un GenUns.t
module Fts = AstLookup.Ft
type fts = ft Fts.t
module GenFts = AstLookup.GenFt
type gen_fts = gen_ft GenFts.t
module Fns = AstLookup.Fn
type fns = declared_fn Fns.t
(*TODO:KILL (just use declared_tys)*)
module ParameterTys = AstLookup.Parameter
type parameter_tys = ty ParameterTys.t
module DeclaredTys = AstLookup.Ty
type declared_tys = ty DeclaredTys.t
type t = {
	rts: rts;
	gen_rts: gen_rts;
	uns: uns;
	gen_uns: gen_uns;
	fts: fts;
	gen_fts: gen_fts;
	fns: fns;
	parameter_tys: parameter_tys;
	declared_tys: declared_tys
}

(*TODO: which of these are really needed?*)
let rt_of_ast({rts; _}: t): Ast.rt -> rt = Rts.get rts
let gen_rt_of_ast({gen_rts; _}: t): Ast.gen_rt -> gen_rt = GenRts.get gen_rts
let un_of_ast({uns; _}: t): Ast.un -> un = Uns.get uns
let gen_un_of_ast({gen_uns; _}: t): Ast.gen_un -> gen_un = GenUns.get gen_uns
let ft_of_ast({fts; _}: t): Ast.ft -> ft = Fts.get fts
let gen_ft_of_ast({gen_fts; _}: t): Ast.gen_ft -> gen_ft = GenFts.get gen_fts
let fn_of_ast({fns; _}: t): Ast.fn -> declared_fn = Fns.get fns

let parameter_ty({parameter_tys; _}: t): Ast.parameter -> ty =
	ParameterTys.get parameter_tys

let set_members(members: modul_member Sym.Lookup.t)({fns; rts; gen_rts; uns; gen_uns; fts; gen_fts; parameter_tys = _; declared_tys = _}: t): unit =
	let set = Sym.Lookup.set members in
	Fns.iter fns begin fun fn_ast fn ->
		set (AstU.fn_name fn_ast) @@ V(Fn(DeclaredFn fn))
	end;
	Rts.iter rts (fun (_, name, _) r -> set name @@ Ty(Rt r));
	GenRts.iter gen_rts (fun (_, name, _, _) r -> set name @@ Ty(GenRt r));
	Uns.iter uns (fun (_, name, _) u -> set name @@ Ty(Un u));
	GenUns.iter gen_uns (fun (_, name, _, _) u -> set name @@ Ty(GenUn u));
	Fts.iter fts (fun (_, name, _) f -> set name @@ Ty(Ft f));
	GenFts.iter gen_fts (fun (_, name, _, _) f -> set name @@ Ty(GenFt f))

let val_of_ast(type_of_ast: t)(ast: Ast.decl_val): v =
	match ast with
	| Ast.Fn f ->
		Fn(DeclaredFn(fn_of_ast type_of_ast f))

let ty_of_ast(type_of_ast: t)(ast: Ast.decl_ty): ty =
	begin match ast with
	| Ast.Rt r ->
		Rt(rt_of_ast type_of_ast r)
	| Ast.GenRt r ->
		GenRt(gen_rt_of_ast type_of_ast r)
	| Ast.Un u ->
		Un(un_of_ast type_of_ast u)
	| Ast.GenUn u ->
		GenUn(gen_un_of_ast type_of_ast u)
	| Ast.Ft f ->
		Ft(ft_of_ast type_of_ast f)
	| Ast.GenFt f ->
		GenFt(gen_ft_of_ast type_of_ast f)
	end

(* Use this for a temporary array value to overwrite later. *)
let nil_array = [||]

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
	let args = ArrayU.map tys @@ declared_ty bindings type_of_ast in
	InstantiateGeneric.f loc ty args

let build(path: Path.t)(full_path: Path.t)(bindings: Bind.t)((_, decls): Ast.modul): modul * t =
	let modul = {path; full_path; members = Sym.Lookup.create()} in
	let rts, gen_rts, uns, gen_uns, fts, gen_fts, fns, parameter_tys, declared_tys =
		Rts.create(), GenRts.create(), Uns.create(), GenUns.create(), Fts.create(), GenFts.create(), Fns.create(), ParameterTys.create(), DeclaredTys.create() in

	(*TODO: rename type_of_ast to something more appropriate*)
	U.returning (modul, {rts; gen_rts; uns; gen_uns; fts; gen_fts; fns; parameter_tys; declared_tys}) @@ fun (modul, type_of_ast) ->
		ArrayU.iter decls begin function
			| Ast.DeclVal v ->
				begin match v with
				| Ast.Fn((_, head, _, _) as fn_ast) ->
					let dummy_code = {bytecodes = nil_array; locs = CodeLocs.empty} in
					let fn =
						match head with
						| Ast.FnPlain _ ->
							let rec fn = {fn_ast; fn_ty; fn_mdl = modul; fn_code = dummy_code}
							and fn_ty = FoG_Ft {ft_origin = FtFromFn fn; return = t_nil; parameters = nil_array} in
							fn
						| Ast.FnGeneric(_, params) ->
							let rec fn = {fn_ast; fn_ty; fn_mdl = modul; fn_code = dummy_code}
							and fn_ty = FoG_Gen({
									gen_ft_origin = GenFtFromFn fn;
									gen_ft_ty_params = params;
									gen_ft_return = t_nil;
									gen_ft_parameters = nil_array;
									gen_ft_cache = N.GenCache.create()
								}) in
							fn in
					Fns.set fns fn_ast fn
				end
			| Ast.DeclTy t ->
				begin match t with
				| Ast.Rt r ->
					Rts.set rts r {rt_id = InstantiateGeneric.next_rt_id(); rt_origin = RtDecl r; properties = nil_array}
				| Ast.GenRt((_, _, params, _) as g) ->
					GenRts.set gen_rts g {gen_rt_origin = g; gen_rt_params = params; gen_rt_properties = nil_array; gen_rt_cache = N.GenCache.create()}
				| Ast.Un u ->
					Uns.set uns u {un_origin = UnDecl u; utys = nil_array}
				| Ast.GenUn((_, _, params, _) as g) ->
					GenUns.set gen_uns g {
						gen_un_origin = g;
						gen_un_params = params;
						gen_un_tys = nil_array;
						gen_un_cache = N.GenCache.create()
					}
				| Ast.Ft(ft_ast) ->
					Fts.set fts ft_ast @@ {ft_origin = FtDecl ft_ast; return = t_nil; parameters = nil_array}
				| Ast.GenFt((_, _, params, _) as g) ->
					GenFts.set gen_fts g @@ {
						gen_ft_origin = GenFtDeclared g;
						gen_ft_ty_params = params;
						gen_ft_return = t_nil;
						gen_ft_parameters = nil_array;
						gen_ft_cache = N.GenCache.create()
					}
				end
		end;
		set_members modul.members type_of_ast;

		let declared_ty: Ast.ty -> ty = declared_ty bindings type_of_ast in

		(* Fill in types *)

		let fill_in_properties(properties: Ast.property array): property array =
			ArrayU.map properties @@ fun (_, name, ty) -> name, declared_ty ty in
		Rts.iter rts (fun (_, _, properties) rt -> rt.properties <- fill_in_properties properties);
		GenRts.iter gen_rts (fun (_, _, _, properties) g -> g.gen_rt_properties <- fill_in_properties properties);

		let fill_in_un_tys(tys: Ast.ty array): ty array =
			ArrayU.map tys declared_ty in
		Uns.iter uns (fun (_, _, tys) un -> un.utys <- fill_in_un_tys tys);
		GenUns.iter gen_uns (fun (_, _, _, tys) g -> g.gen_un_tys <- fill_in_un_tys tys);

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
		GenFts.iter gen_fts (fun (_, _, _, signature) -> fill_in_gen_ft signature);

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
