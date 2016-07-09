type t = Binding.t AstU.AccessLookup.t
let binding = AstU.AccessLookup.get

type ctx = {
	scope: Scope.t;
	add: Ast.access -> Binding.t -> unit
}

let add_type({scope; add}: ctx)(typ: Ast.typ): unit = (*TODO:NAME*)
	match typ with
	| Ast.TypeAccess((loc, name) as access) ->
		add access @@ ScopeU.get scope loc name

let rec bind_cases({scope; _} as ctx: ctx)(cases: Ast.cs_part array): unit =
	ArrayU.iter cases begin fun (_, test, result) ->
		match test with
		| Ast.AtTest(_, typ, declare) ->
			add_type ctx typ;
			let scope = ScopeU.add_local scope declare in
			bind_expr {ctx with scope} result
	end

and bind_expr({scope; add} as ctx: ctx)(expr: Ast.expr): unit =
	let recur = bind_expr ctx in
	(*TODO: open Ast*)
	match expr with
	| Ast.At(_, typ, expr) ->
	 	add_type ctx typ;
		recur expr
	| Ast.ExprType(typ) ->
		add_type ctx typ
	| Ast.ExprAccess((loc, name) as access) ->
		add access @@ ScopeU.get scope loc name
	| Ast.Call(_, called, arguments) ->
		recur called;
		ArrayU.iter arguments recur
	| Ast.Cs(_, cased, cases) ->
		recur cased;
		bind_cases ctx cases
	| Ast.Let(_, declare, value, expr) ->
		recur value;
		let scope = ScopeU.add_local scope declare in
		bind_expr {ctx with scope} expr
	| Ast.Literal _ ->
		()
	| Ast.Seq(_, a, b) ->
		recur a;
		recur b
	| Ast.Partial(_, f, args) ->
		recur f;
		ArrayU.iter args recur
	| Ast.Quote(_, _, parts) ->
		ArrayU.iter parts @@ fun (expr, _) -> recur expr
	| Ast.Check(_, expr) ->
		recur expr

let bind((_, decls): Ast.modul): t =
	AstU.AccessLookup.build begin fun add ->
		let base_scope = ScopeU.get_base decls in
		let ctx = {scope = base_scope; add} in

		let bind_signature((_, return_type, parameters): Ast.signature): unit =
			add_type ctx return_type;
			ArrayU.iter parameters @@ fun (_, _, typ) -> add_type ctx typ in

		ArrayU.iter decls begin function
		| Ast.Fn((_, _, ((_, _, params) as signature), body)) ->
			bind_signature signature;
			let scope = ScopeU.add_params base_scope params in
			bind_expr {ctx with scope} body

		| Ast.Cn((_, _, typ, cases)) ->
			add_type ctx typ;
			bind_cases ctx cases

		| Ast.Rt((_, _, properties)) ->
			ArrayU.iter properties @@ fun (_, _, typ) -> add_type ctx typ

		| Ast.Un((_, _, types)) ->
			ArrayU.iter types @@ add_type ctx

		| Ast.Ft((_, _, signature)) ->
			bind_signature signature

		| Ast.Ct((_, _, cases)) ->
			ArrayU.iter cases @@ fun (return, input) ->
				add_type ctx return;
				add_type ctx input
		end
	end

let output accesses =
	AstU.AccessLookup.output AstU.output_access BindingU.output accesses
