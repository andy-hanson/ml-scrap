open Ast

let typ_loc = function
	| TypeAccess(Access(loc, _)) | Or(loc, _) ->
		loc

let expr_loc = function
	| ExprAccess(Access(loc, _)) | Call(loc, _, _) | Case(loc, _, _) | Let(loc, _, _, _) | Literal(loc, _) | Seq(loc, _, _) ->
		loc

let decl_loc_name = function
	| DeclFn(Fn(loc, name, _, _)) ->
		loc, name
	| DeclRc(Rc(loc, name, _)) ->
		loc, name

module type SimpleKey = sig
	type t
	val loc: t -> Loc.t
end
module AstLookup(K: SimpleKey): Lookup.S with type key = K.t = Lookup.Make(struct
	type t = K.t
	let equal = (==)
	let hash ast = Loc.hash (K.loc ast)
end)

module AccessLookup = AstLookup(struct
	type t = access
	let loc(Access(loc, _)) = loc
end)
module ExprLookup = AstLookup(struct
	type t = expr
	let loc = expr_loc
end)
module FnLookup = AstLookup(struct
	type t = fn
	let loc(Fn(loc, _, _, _)) = loc
end)
module RcLookup = AstLookup(struct
	type t = rc
	let loc(Rc(loc, _, _)) = loc
end)
module LocalDeclareLookup = AstLookup(struct
	type t = local_declare
	let loc(LocalDeclare(loc, _)) = loc
end)
module ParameterLookup = AstLookup(struct
	type t = parameter
	let loc(Parameter(loc, _, _)) = loc
end)

let modul_split(decls: decl array): fn array * rc array =
	let fns = BatDynArray.create() in
	let rcs = BatDynArray.create() in
	ArrayU.iter decls begin function
		| DeclFn fn ->
			BatDynArray.add fns fn
		| DeclRc rc ->
			BatDynArray.add rcs rc
	end;
	BatDynArray.to_array fns, BatDynArray.to_array rcs

let modul_fns(decls: decl array): fn array =
	fst (modul_split decls)

(*TODO: ordering in this file (and in mli)*)
let output_access(out: 'o OutputU.t)(Access(_, name)): unit =
	Symbol.output out name

let rec output_typ(out: 'o OutputU.t)(typ: typ): unit =
	match typ with
	| TypeAccess(access) ->
		output_access out access
	| Or(_, types) ->
		OutputU.out out "Or(%a)" (OutputU.out_array output_typ) types

let output_parameter(out: 'o OutputU.t)(Parameter(_, name, typ)): unit =
	OutputU.out out "%a %a" Symbol.output name output_typ typ

let output_local_declare(out: 'o OutputU.t)(LocalDeclare(_, name)): unit =
	OutputU.out out "%a" Symbol.output name

let rec output_expr(out: 'o OutputU.t)(expr: expr): unit =
	match expr with
	| ExprAccess(access) ->
		output_access out access
	| Call(_, fn, args) ->
		OutputU.out out "Call %a %a" output_expr fn (OutputU.out_array output_expr) args
	| Case(_, cased, parts) ->
		let output_part out (CasePart(_, test, expr)) =
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

(*TODO
let output_decl_val_kind out kind =
	match kind with
	| Fn (sign, body) ->
		let out_sig out (Signature(_, typ, params)) =
			let out_param out (Parameter(_, sym, typ)) =
				OutputU.out out "%a %a" Symbol.output sym output_typ typ in
			OutputU.out out "%a %a" output_typ typ (OutputU.out_array out_param) params in
		OutputU.out out "fn %a %a" out_sig sign output_expr body
let output_decl_val(out: 'o OutputU.t)(DeclVal(_, symbol, kind)): unit =
	OutputU.out out "DeclVal(%a, %a)" Symbol.output symbol output_decl_val_kind kind

let output_decl_type_kind out kind =
	match kind with
	| Rc _ ->
		OutputU.str out "Rc..."
let output_decl_type(out: 'o OutputU.t)(DeclType(_, symbol, kind)): unit =
	OutputU.out out "DeclType(%a, %a)" Symbol.output symbol output_decl_type_kind kind

let output_decl(out: 'o OutputU.t)(decl: decl): unit =
	match decl with
	| Val v -> output_decl_val out v
	| Type t -> output_decl_type out t
*)

let output_fn(out: 'o OutputU.t)(Fn(_, name, sign, body)): unit =
	let out_sig out (Signature(_, typ, params)) =
		let out_param out (Parameter(_, sym, typ)) =
			OutputU.out out "%a %a" Symbol.output sym output_typ typ in
		OutputU.out out "%a %a" output_typ typ (OutputU.out_array out_param) params in
	OutputU.out out "fn %a %a %a" Symbol.output name out_sig sign output_expr body

let output_rc(out: 'o OutputU.t)(Rc(_, _, _)): unit =
	OutputU.str out "Rc..."

let output_decl(out: 'o OutputU.t)(decl: decl): unit =
	match decl with
	| DeclFn fn ->
		output_fn out fn
	| DeclRc rc ->
		output_rc out rc

let output_modul(out: 'o OutputU.t)(Modul(decls)): unit =
	OutputU.out_array output_decl out decls
