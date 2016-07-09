open Ast

let typ_loc = function
	| TypeAccess(loc, _) ->
		loc

let expr_loc = function
	| ExprType(typ) ->
		typ_loc typ
	| At(loc, _, _) | ExprAccess(loc, _) | Call(loc, _, _) | Cs(loc, _, _) | Let(loc, _, _, _)
	| Literal(loc, _) | Seq(loc, _, _) | Partial(loc, _, _) | Quote(loc, _, _) | Check(loc, _) ->
		loc

let decl_loc_name = function
	| Fn((loc, name, _, _)) ->
		loc, name
	| Cn((loc, name, _, _)) ->
		loc, name
	| Rt((loc, name, _)) ->
		loc, name
	| Un((loc, name, _)) ->
		loc, name
	| Ft((loc, name, _)) ->
		loc, name
	| Ct((loc, name, _)) ->
		loc, name

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
module CnLookup = AstLookup(struct
	type t = cn
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
module CtLookup = AstLookup(struct
	type t = ct
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

let output_typ(out: 'o OutputU.t)(typ: typ): unit =
	match typ with
	| TypeAccess(access) ->
		output_access out access

let output_parameter(out: 'o OutputU.t)((_, name, typ): Ast.parameter): unit =
	OutputU.out out "%a %a" Sym.output name output_typ typ

let output_local_declare(out: 'o OutputU.t)((_, name): Ast.local_declare): unit =
	OutputU.out out "%a" Sym.output name

let rec output_expr(out: 'o OutputU.t)(expr: expr): unit =
	match expr with
	| At(_, typ, expr) ->
		OutputU.out out "At(%a, %a)"
			output_typ typ
			output_expr expr
	| ExprType(typ) ->
		OutputU.out out "ExprType(%a)"
			output_typ typ
	| ExprAccess(access) ->
		output_access out access
	| Call(_, fn, args) ->
		OutputU.out out "Call(%a, %a)"
			output_expr fn
			(OutputU.out_array output_expr) args
	| Cs(_, cased, parts) ->
		let output_part out (_, test, expr) =
			let out_test out (AtTest(_, typ, declare)) =
				OutputU.out out "AtTest(%a, %a)"
					output_typ typ
					output_local_declare declare in
			OutputU.out out "CsPart(%a, %a)"
				out_test test
				output_expr expr in
		OutputU.out out "Cs(%a, %a)"
			output_expr cased
			(OutputU.out_array output_part) parts
	| Let(_, declare, value, expr) ->
		OutputU.out out "Let(%a = %a, %a)"
			output_local_declare declare
			output_expr value
			output_expr expr
	| Literal(_, v) ->
		ValU.output_primitive out v
	| Seq(_, a, b) ->
		OutputU.out out "Seq(%a, %a)"
			output_expr a
			output_expr b
	| Partial(_, fn, args) ->
		OutputU.out out "Partial(%a, %a)"
			output_expr fn
			(OutputU.out_array output_expr) args
	| Quote(_, head, parts) ->
		let output_part(out: 'o OutputU.t)((expr, str): Ast.quote_part): unit =
			OutputU.out out "%a \"%s\""
				output_expr expr
				(String.escaped str) in
		OutputU.out out "Quote(\"%s\", %a)"
			(String.escaped head)
			(OutputU.out_array output_part) parts
	| Check(_, checked) ->
		OutputU.out out "Check(%a)" output_expr checked

let output_fn(out: 'o OutputU.t)((_, name, sign, body): Ast.fn): unit =
	let out_sig out (_, typ, params) =
		let out_param out (_, sym, typ) =
			OutputU.out out "%a %a" Sym.output sym output_typ typ in
		OutputU.out out "%a %a" output_typ typ (OutputU.out_array out_param) params in
	OutputU.out out "fn %a %a %a" Sym.output name out_sig sign output_expr body

let output_cn(_out: 'o OutputU.t)((_, _, _, _): Ast.cn): unit =
	raise U.TODO

let output_rt(_out: 'o OutputU.t)((_, _, _): Ast.rt): unit =
	raise U.TODO

let output_un(_out: 'o OutputU.t)((_, _, _): Ast.un): unit =
	raise U.TODO

let output_ft(_out: 'o OutputU.t)((_, _, _): Ast.ft): unit =
	raise U.TODO

let output_ct(_out: 'o OutputU.t)((_, _, _): Ast.ct): unit =
	raise U.TODO

let output_decl(out: 'o OutputU.t)(decl: decl): unit =
	match decl with
	| Fn fn -> output_fn out fn
	| Cn cn -> output_cn out cn
	| Rt rt -> output_rt out rt
	| Un un -> output_un out un
	| Ft ft -> output_ft out ft
	| Ct ct -> output_ct out ct

let output_modul(out: 'o OutputU.t)((_imports, decls): modul): unit =
	OutputU.out_array output_decl out decls
