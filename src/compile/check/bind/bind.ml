module Accesses = AstU.AccessLookup
type t = Binding.t Accesses.t
let binding accesses = Accesses.get accesses

let do_bind(base_scope: Scope.t)(add: Ast.access -> Binding.t -> unit)(decls: Ast.decl array): unit =
	let rec add_type(typ: Ast.typ): unit = (*TODO:NAME*)
		match typ with
		| Ast.TypeAccess(Ast.Access(loc, name) as access) ->
			add access (ScopeU.get base_scope loc name)
		| Ast.Or(_, parts) ->
		  ArrayU.iter parts add_type in

	let rec bind_expr(scope: Scope.t)(expr: Ast.expr): unit =
		match expr with
		| Ast.ExprAccess(Ast.Access(loc, name) as access) ->
			add access (ScopeU.get scope loc name)
		| Ast.Call(_, called, arguments) ->
			bind_expr scope called;
			ArrayU.iter arguments (bind_expr scope)
		| Ast.Case(_, cased, cases) ->
			bind_expr scope cased;
			ArrayU.iter cases begin fun (Ast.CasePart(_, test, result)) ->
				match test with
				| Ast.AsTest(_, declare, typ) ->
					add_type typ;
					let scope = ScopeU.add_local scope declare in
					bind_expr scope result
			end
		| Ast.Let(_, declare, value, expr) ->
			bind_expr scope value;
			let scope = ScopeU.add_local scope declare in
			bind_expr scope expr
		| Ast.Literal _ ->
			()
		| Ast.Seq(_, a, b) ->
			bind_expr scope a;
			bind_expr scope b in

	ArrayU.iter decls begin function
		| Ast.DeclFn(Ast.Fn(_, _, Ast.Signature(_, return_type, params), body)) ->
			add_type return_type;
			ArrayU.iter params (fun (Ast.Parameter(_, _, typ)) -> add_type typ);
			bind_expr (ScopeU.add_params base_scope params) body

		| Ast.DeclRc(Ast.Rc(_, _, properties)) ->
			ArrayU.iter properties (fun (Ast.Property(_, _, typ)) -> add_type typ)
	end

let bind(ctx: CompileContext.t)(Ast.Modul(decls)): t =
	U.returning (Accesses.create()) begin fun accesses ->
		let base_scope = ScopeU.get_base ctx decls in
		do_bind base_scope (Accesses.set accesses) decls
	end

let output accesses = Accesses.output AstU.output_access BindingU.output accesses
