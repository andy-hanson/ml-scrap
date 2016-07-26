open Ast

let ty_loc(ty: ty): Loc.t =
 	match ty with
	| TyAccess(loc, _) | TyInst(loc, _, _) ->
		loc

let expr_loc(expr: expr): Loc.t =
	match expr with
	| ExprType(ty) ->
		ty_loc ty
	| At(loc, _, _, _) | ExprAccess(loc, _) | Call(loc, _, _) | Cs(loc, _, _) | GetProperty(loc, _, _)
	| Let(loc, _, _, _) | Literal(loc, _) | Seq(loc, _, _) | Partial(loc, _, _) | Quote(loc, _, _)
	| Check(loc, _) | GenInst(loc, _, _) ->
		loc

(*let name_sym(n: ty_name): Sym.t =
	match n with
	| Plain n -> n
	| Generic(n, _) -> n*)

let fn_name((_, head, _, _): fn): Sym.t =
	begin match head with
	| FnPlain name -> name
	| FnGeneric(name, _) -> name
	end

let decl_loc_name(decl: decl): Loc.t * Sym.t =
	match decl with
	| DeclVal v ->
		begin match v with
		| Fn((loc, _, _, _) as fn) ->
			loc, fn_name fn
		end
	| DeclTy t ->
		begin match t with
		| Rt(loc, name, _) | GenRt(loc, name, _, _) | Un((loc, name, _)) | Ft((loc, name, _)) ->
			loc, name
		end
