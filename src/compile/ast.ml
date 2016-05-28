type typ =
	| TypeAccess of Loc.t * Symbol.t

type local_declare = LocalDeclare of Loc.t * Symbol.t * typ

type expr_kind =
	| Access of Symbol.t
	| Call of expr * expr array
	| Let of local_declare * expr * expr
	| Literal of Val.t
	| Seq of expr * expr
and expr = Expr of Loc.t * expr_kind

type property = Property of Loc.t * Symbol.t * typ

type signature = Signature of Loc.t * typ * local_declare array

type decl_val_kind =
	| Fn of signature * expr
type decl_val = DeclVal of Loc.t * Symbol.t * decl_val_kind

type decl_type_kind =
	| Rec of property array
type decl_type = DeclType of Loc.t * Symbol.t * decl_type_kind

type decl =
	| Val of decl_val
	| Type of decl_type

type modul = Modul of Loc.t * (decl array)


(* boilerplate *)

let output_typ(out: 'a BatIO.output)(typ: typ): unit =
	match typ with
	| TypeAccess(_, sym) ->
		Symbol.output out sym


let output_local_declare(out: 'a BatIO.output)(LocalDeclare(_, name, typ)): unit =
	OutputU.out out "%a: %a" Symbol.output name output_typ typ

let output_decl_val_kind(out: 'a BatIO.output)(Fn(signature, expr)): unit =
	OutputU.out out "Fn..."
let output_decl_val(out: 'a BatIO.output)(DeclVal(_, name, kind)): unit =
	OutputU.out out "DeclVal(%a, %a)" Symbol.output name output_decl_val_kind kind


let rec output_expr(out: 'a BatIO.output)(Expr(_, kind)): unit =
	match kind with
	| Access sym ->
		Symbol.output out sym
	| Call(fn, args) ->
		OutputU.out out "Call %a %a" output_expr fn (OutputU.out_array output_expr) args
	| Let(declare, value, expr) ->
		OutputU.out out "Let(%a = %a; %a)" output_local_declare declare output_expr value output_expr expr
	| Literal v ->
		Val.output out v
	| Seq(a, b) ->
		OutputU.out out "Seq(%a; %a)" output_expr a output_expr b


let output_decl_val_kind out kind =
	match kind with
	| Fn (sign, body) ->
		let out_sig out (Signature(_, typ, params)) =
			let out_param out (LocalDeclare(_, sym, typ)) =
				OutputU.out out "%a %a" Symbol.output sym output_typ typ in
			OutputU.out out "%a %a" output_typ typ (OutputU.out_array out_param) params in
		OutputU.out out "fn %a %a" out_sig sign output_expr body
let output_decl_val(out: 'a BatIO.output)(DeclVal(_, symbol, kind)): unit =
	OutputU.out out "DeclVal(%a, %a)" Symbol.output symbol output_decl_val_kind kind

let output_decl_type_kind out kind =
	match kind with
	| Rec _ ->
		BatIO.nwrite out "Rec..."
let output_decl_type(out: 'a BatIO.output)(DeclType(_, symbol, kind)): unit =
	OutputU.out out "DeclType(%a, %a)" Symbol.output symbol output_decl_type_kind kind

let output_decl(out: 'a BatIO.output)(decl: decl): unit =
	match decl with
	| Val v -> output_decl_val out v
	| Type t -> output_decl_type out t

let output_modul(out: 'a BatIO.output)(Modul(_, decls)): unit =
	OutputU.out_array output_decl out decls
