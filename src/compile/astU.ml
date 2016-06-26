open Ast

let typ_loc = function
	| TypeAccess(loc, _) ->
		loc

let expr_loc = function
	| ExprAccess(loc, _) | Call(loc, _, _) | Case(loc, _, _) | Let(loc, _, _, _) | Literal(loc, _) | Seq(loc, _, _) ->
		loc

let decl_loc_name = function
	| DeclFn((loc, name, _, _)) ->
		loc, name
	| DeclCn((loc, name, _, _)) ->
		loc, name
	| DeclRt((loc, name, _)) ->
		loc, name
	| DeclUn((loc, name, _)) ->
		loc, name
	| DeclFt((loc, name, _)) ->
		loc, name
	| DeclCt((loc, name, _)) ->
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

(*TODO:KILL!KILL!KILL!*)
let modul_split(decls: decl array): Ast.decl array * Ast.decl array =
	(*TODO: ArrayU.split helper*)
	let values = MutArray.create() in
	let types = MutArray.create() in
	ArrayU.iter decls begin fun decl ->
		match decl with
		| DeclFn _ | DeclCn _ ->
			MutArray.add values decl
		| DeclRt _ | DeclUn _ | DeclFt _ | DeclCt _ ->
			MutArray.add types decl
	end;
	MutArray.to_array values, MutArray.to_array types

(*TODO:KILL!KILL!KILL!*)
let modul_fns(decls: decl array): fn array =
	ArrayU.filter_map decls begin function
		| Ast.DeclFn f -> Some f
		| _ -> None
	end

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
	| ExprAccess(access) ->
		output_access out access
	| Call(_, fn, args) ->
		OutputU.out out "Call %a %a" output_expr fn (OutputU.out_array output_expr) args
	| Case(_, cased, parts) ->
		let output_part out (_, test, expr) =
			let out_test out (AsTest(_, declare, typ)) =
				OutputU.out out "AsTest(%a, %a)" output_local_declare declare output_typ typ in
			OutputU.out out "CasePart(%a, %a)" out_test test output_expr expr in
		OutputU.out out "Case(%a, %a)" output_expr cased (OutputU.out_array output_part) parts
	| Let(_, declare, value, expr) ->
		OutputU.out out "Let(%a = %a; %a)" output_local_declare declare output_expr value output_expr expr
	| Literal(_, v) ->
		ValU.output out v
	| Seq(_, a, b) ->
		OutputU.out out "Seq(%a; %a)" output_expr a output_expr b

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
	| DeclFn fn -> output_fn out fn
	| DeclCn cn -> output_cn out cn
	| DeclRt rt -> output_rt out rt
	| DeclUn un -> output_un out un
	| DeclFt ft -> output_ft out ft
	| DeclCt ct -> output_ct out ct

let output_modul(out: 'o OutputU.t)(Modul(decls)): unit =
	OutputU.out_array output_decl out decls
