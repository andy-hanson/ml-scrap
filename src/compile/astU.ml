module type SimpleKey = sig
	type t
	val loc: t -> Loc.t
end
module AstLookup(K: SimpleKey): Lookup.S with type key = K.t = Lookup.Make(struct
	type t = K.t
	let equal = (==)
	let hash ast = Loc.hash (K.loc ast)
end)

module ExprLookup = AstLookup(struct
	type t = Ast.expr
	let loc = Ast.expr_loc
end)
module TypLookup = AstLookup(struct
	type t = Ast.typ
	let loc(Ast.TypeAccess(loc, _)) = loc
end)
(*TODO: probably never used*)
module DeclLookup = AstLookup(struct
	type t = Ast.decl
	let loc _ = raise U.TODO
end)
module FnLookup = AstLookup(struct
	type t = Ast.fn
	let loc(Ast.Fn(loc, _, _, _)) = loc
end)
module RcLookup = AstLookup(struct
	type t = Ast.rc
	let loc(Ast.Rc(loc, _, _)) = loc
end)
module LocalDeclareLookup = AstLookup(struct
	type t = Ast.local_declare
	let loc(Ast.LocalDeclare(loc, _)) = loc
end)
module ParameterLookup = AstLookup(struct
	type t = Ast.parameter
	let loc(Ast.Parameter(loc, _, _)) = loc
end)

let modul_rcs(decls: Ast.decl array): Ast.rc array =
	decls |> BatArray.filter_map begin function
		| Ast.DeclRc r ->
			Some r
		| _ ->
			None
	end

let output_typ(out: 'o OutputU.t)(typ: Ast.typ): unit =
	match typ with
	| Ast.TypeAccess(_, sym) ->
		Symbol.output out sym

let output_parameter(out: 'o OutputU.t)(Ast.Parameter(_, name, typ)): unit =
	OutputU.out out "%a %a" Symbol.output name output_typ typ

let output_local_declare(out: 'o OutputU.t)(Ast.LocalDeclare(_, name)): unit =
	OutputU.out out "%a" Symbol.output name

let rec output_expr(out: 'o OutputU.t)(expr: Ast.expr): unit =
	match expr with
	| Ast.Access(_, sym) ->
		Symbol.output out sym
	| Ast.Call(_, fn, args) ->
		OutputU.out out "Call %a %a" output_expr fn (OutputU.out_array output_expr) args
	| Ast.Let(_, declare, value, expr) ->
		OutputU.out out "Let(%a = %a; %a)" output_local_declare declare output_expr value output_expr expr
	| Ast.Literal(_, v) ->
		Val.output out v
	| Ast.Seq(_, a, b) ->
		OutputU.out out "Seq(%a; %a)" output_expr a output_expr b

(*TODO
let output_decl_val_kind out kind =
	match kind with
	| Ast.Fn (sign, body) ->
		let out_sig out (Ast.Signature(_, typ, params)) =
			let out_param out (Ast.Parameter(_, sym, typ)) =
				OutputU.out out "%a %a" Symbol.output sym output_typ typ in
			OutputU.out out "%a %a" output_typ typ (OutputU.out_array out_param) params in
		OutputU.out out "fn %a %a" out_sig sign output_expr body
let output_decl_val(out: 'o OutputU.t)(Ast.DeclVal(_, symbol, kind)): unit =
	OutputU.out out "DeclVal(%a, %a)" Symbol.output symbol output_decl_val_kind kind

let output_decl_type_kind out kind =
	match kind with
	| Ast.Rc _ ->
		OutputU.str out "Rc..."
let output_decl_type(out: 'o OutputU.t)(Ast.DeclType(_, symbol, kind)): unit =
	OutputU.out out "DeclType(%a, %a)" Symbol.output symbol output_decl_type_kind kind

let output_decl(out: 'o OutputU.t)(decl: Ast.decl): unit =
	match decl with
	| Ast.Val v -> output_decl_val out v
	| Ast.Type t -> output_decl_type out t
*)

let output_fn(out: 'o OutputU.t)(Ast.Fn(_, name, sign, body)): unit =
	let out_sig out (Ast.Signature(_, typ, params)) =
		let out_param out (Ast.Parameter(_, sym, typ)) =
			OutputU.out out "%a %a" Symbol.output sym output_typ typ in
		OutputU.out out "%a %a" output_typ typ (OutputU.out_array out_param) params in
	OutputU.out out "fn %a %a %a" Symbol.output name out_sig sign output_expr body

let output_rc(out: 'o OutputU.t)(Ast.Rc(_, _, _)): unit =
	OutputU.str out "Rc..."

let output_decl(out: 'o OutputU.t)(decl: Ast.decl): unit =
	match decl with
	| Ast.DeclFn fn ->
		output_fn out fn
	| Ast.DeclRc rc ->
		output_rc out rc

let output_modul(out: 'o OutputU.t)(Ast.Modul(decls)): unit =
	OutputU.out_array output_decl out decls
