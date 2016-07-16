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

let name_sym(n: ty_name): Sym.t =
	match n with
	| Plain n -> n
	| Generic(n, _) -> n

let decl_loc_name(decl: decl): Loc.t * Sym.t =
	match decl with
	| DeclVal v ->
		begin match v with
		| Fn((loc, name, _, _)) ->
			loc, name
		end
	| DeclTy t ->
		begin match t with
		| Rt((loc, name, _)) ->
			loc, name
		| Un((loc, name, _)) ->
			loc, name
		| Ft((loc, name, _)) ->
			loc, name_sym name
		end

module type SimpleKey = sig
	type t
	val loc: t -> Loc.t
end
module AstLookup(K: SimpleKey): Lookup.S with type key = K.t = Lookup.Make(struct
	type t = K.t
	let equal = (==)
	let hash ast = Loc.hash @@ K.loc ast
end)

module AccessLookup = AstLookup(struct
	type t = access
	let loc(loc, _) = loc
end)
module ExprLookup = AstLookup(struct
	type t = expr
	let loc = expr_loc
end)
module FnLookup = AstLookup(struct
	type t = fn
	let loc(loc, _, _, _) = loc
end)
module RtLookup = AstLookup(struct
	type t = rt
	let loc(loc, _, _) = loc
end)
module UnLookup = AstLookup(struct
	type t = un
	let loc(loc, _, _) = loc
end)
module FtLookup = AstLookup(struct
	type t = ft
	let loc(loc, _, _) = loc
end)
module LocalDeclareLookup = AstLookup(struct
	type t = local_declare
	let loc(loc, _) = loc
end)
module ParameterLookup = AstLookup(struct
	type t = parameter
	let loc(loc, _, _) = loc
end)

(*TODO: ordering in this file (and in mli)*)
let output_access(out: 'o OutputU.t)((_, name): Ast.access): unit =
	Sym.output out name

let output_ty(out: 'o OutputU.t)(ty: ty): unit =
	match ty with
	| TyAccess(access) ->
		output_access out access
	| TyInst(_, _, _) ->
		raise U.TODO

let output_parameter(out: 'o OutputU.t)((_, name, ty): Ast.parameter): unit =
	OutputU.out out "%a %a" Sym.output name output_ty ty

let output_local_declare(out: 'o OutputU.t)((_, name): Ast.local_declare): unit =
	OutputU.out out "%a" Sym.output name

let output_literal(out: 'o OutputU.t)(v: Ast.literal_value): unit =
	let o fmt = OutputU.out out fmt in
	begin match v with
	| Int i -> o "%i" i
	| Float f -> o "%f" f
	| String s -> o "\"%s\"" @@ String.escaped s
	end

let rec output_pattern(out: 'o OutputU.t)(pattern: pattern): unit =
	let o fmt = OutputU.out out fmt in
	match pattern with
	| PSingle declare ->
		o "PSingle(%a)" output_local_declare declare
	| PDestruct(_, patterns) ->
		o "PDestruct(%a)" (OutputU.out_array output_pattern) patterns

let rec output_expr(out: 'o OutputU.t)(expr: expr): unit =
	let o fmt = OutputU.out out fmt in
	match expr with
	| At(_, kind, ty, expr) ->
		o "At(%s, %a, %a)"
			(match kind with | Ast.Exact -> "Exact" | Ast.Convert -> "Convert")
			output_ty ty
			output_expr expr
	| ExprType(ty) ->
		o "ExprType(%a)"
			output_ty ty
	| ExprAccess(access) ->
		output_access out access
	| Call(_, fn, args) ->
		o "Call(%a, %a)"
			output_expr fn
			(OutputU.out_array output_expr) args
	| Cs(_, cased, parts) ->
		let output_part(out: 'o OutputU.t)((_, (_, ty, pattern), expr): cs_part): unit =
			OutputU.out out "CsPart((%a, %a), %a)"
				output_ty ty
				output_pattern pattern
				output_expr expr in
		o "Cs(%a, %a)"
			output_expr cased
			(OutputU.out_array output_part) parts
	| GetProperty(_, expr, property) ->
		o "GetProperty(%a, %a)"
			output_expr expr
			Sym.output property
	| Let(_, pattern, value, expr) ->
		o "Let(%a = %a, %a)"
			output_pattern pattern
			output_expr value
			output_expr expr
	| Literal(_, v) ->
		output_literal out v
	| Seq(_, a, b) ->
		o "Seq(%a, %a)"
			output_expr a
			output_expr b
	| Partial(_, fn, args) ->
		o "Partial(%a, %a)"
			output_expr fn
			(OutputU.out_array output_expr) args
	| Quote(_, head, parts) ->
		let output_part(out: 'o OutputU.t)((expr, str): Ast.quote_part): unit =
			OutputU.out out "%a \"%s\""
				output_expr expr
				(String.escaped str) in
		o "Quote(\"%s\", %a)"
			(String.escaped head)
			(OutputU.out_array output_part) parts
	| Check(_, checked) ->
		o "Check(%a)" output_expr checked
	| GenInst(_, expr, tys) ->
		o "GenInst(%a, %a)"
			output_expr expr
			(OutputU.out_array output_ty) tys

let output_fn(out: 'o OutputU.t)((_, name, sign, body): Ast.fn): unit =
	let out_sig(out: 'o OutputU.t)((_, ty, params): Ast.signature): unit =
		let out_param(out: 'o OutputU.t)((_, sym, ty): Ast.parameter): unit =
			OutputU.out out "%a %a" Sym.output sym output_ty ty in
		OutputU.out out "%a %a" output_ty ty (OutputU.out_array out_param) params in
	OutputU.out out "fn %a %a %a" Sym.output name out_sig sign output_expr body

let output_rt(_out: 'o OutputU.t)((_, _, _): Ast.rt): unit =
	raise U.TODO

let output_un(_out: 'o OutputU.t)((_, _, _): Ast.un): unit =
	raise U.TODO

let output_ft(_out: 'o OutputU.t)((_, _, _): Ast.ft): unit =
	raise U.TODO

let output_decl_val(out: 'o OutputU.t)(decl: decl_val): unit =
	begin match decl with
	| Fn fn -> output_fn out fn
	end

let output_decl_ty(out: 'o OutputU.t)(decl: decl_ty): unit =
	begin match decl with
	| Rt rt -> output_rt out rt
	| Un un -> output_un out un
	| Ft ft -> output_ft out ft
	end

let output_decl(out: 'o OutputU.t)(decl: decl): unit =
	match decl with
	| DeclVal v -> output_decl_val out v
	| DeclTy t -> output_decl_ty out t

let output_modul(out: 'o OutputU.t)((_imports, decls): modul): unit =
	OutputU.out_array output_decl out decls
