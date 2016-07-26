open Ast

let output_access(out: 'o OutputU.t)((_, name): access): unit =
	Sym.output out name

let output_ty(out: 'o OutputU.t)(ty: ty): unit =
	match ty with
	| TyAccess(access) ->
		output_access out access
	| TyInst(_, _, _) ->
		U.todo()

let output_parameter(out: 'o OutputU.t)((_, name, ty): parameter): unit =
	OutputU.out out "%a %a" Sym.output name output_ty ty

let output_ty_param(out: 'o OutputU.t)((_, name): ty_param): unit =
	OutputU.out out "%a" Sym.output name

let output_local_declare(out: 'o OutputU.t)((_, name): local_declare): unit =
	OutputU.out out "%a" Sym.output name

let output_literal(out: 'o OutputU.t)(v: literal_value): unit =
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
		o "PDestruct(%a)" (ArrayU.output output_pattern) patterns

let rec output_expr(out: 'o OutputU.t)(expr: expr): unit =
	let o fmt = OutputU.out out fmt in
	match expr with
	| At(_, kind, ty, expr) ->
		o "At(%s, %a, %a)"
			(match kind with | Exact -> "Exact" | Convert -> "Convert")
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
			(ArrayU.output output_expr) args
	| Cs(_, cased, parts) ->
		let output_part(out: 'o OutputU.t)((_, (_, ty, pattern), expr): cs_part): unit =
			OutputU.out out "CsPart((%a, %a), %a)"
				output_ty ty
				output_pattern pattern
				output_expr expr in
		o "Cs(%a, %a)"
			output_expr cased
			(ArrayU.output output_part) parts
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
			(ArrayU.output output_expr) args
	| Quote(_, head, parts) ->
		let output_part(out: 'o OutputU.t)((expr, str): quote_part): unit =
			OutputU.out out "%a \"%s\""
				output_expr expr
				(String.escaped str) in
		o "Quote(\"%s\", %a)"
			(String.escaped head)
			(ArrayU.output output_part) parts
	| Check(_, checked) ->
		o "Check(%a)" output_expr checked
	| GenInst(_, expr, tys) ->
		o "GenInst(%a, %a)"
			output_expr expr
			(ArrayU.output output_ty) tys

let output_fn(_out: 'o OutputU.t)((_, _name, _sign, _body): fn): unit =
	let _out_sig(out: 'o OutputU.t)((_, ty, params): signature): unit =
		let out_param(out: 'o OutputU.t)((_, sym, ty): parameter): unit =
			OutputU.out out "%a %a" Sym.output sym output_ty ty in
		OutputU.out out "%a %a" output_ty ty (ArrayU.output out_param) params in
	U.todo()
	(*OutputU.out out "fn %a %a %a" Sym.output name out_sig sign output_expr body*)

let output_rt(_out: 'o OutputU.t)((_, _, _): rt): unit =
	U.todo()

let output_un(_out: 'o OutputU.t)((_, _, _): un): unit =
	U.todo()

let output_ft(_out: 'o OutputU.t)((_, _, _): ft): unit =
	U.todo()

let output_decl_val(out: 'o OutputU.t)(decl: decl_val): unit =
	begin match decl with
	| Fn fn -> output_fn out fn
	end

let output_decl_ty(out: 'o OutputU.t)(decl: decl_ty): unit =
	begin match decl with
	| Rt rt -> output_rt out rt
	| GenRt _ -> U.todo()
	| Un un -> output_un out un
	| Ft ft -> output_ft out ft
	end

let output_decl(out: 'o OutputU.t)(decl: decl): unit =
	match decl with
	| DeclVal v -> output_decl_val out v
	| DeclTy t -> output_decl_ty out t

let output_modul(out: 'o OutputU.t)((imports, decls): modul): unit =
	assert (ArrayU.empty imports); (*TODO*)
	ArrayU.output output_decl out decls
