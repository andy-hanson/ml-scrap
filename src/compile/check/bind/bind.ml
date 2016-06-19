type t = Binding.t AstU.AccessLookup.t
let binding = AstU.AccessLookup.get

let bind(decls: Ast.decl array): t =
	AstU.AccessLookup.build begin fun add ->
		let base_scope = ScopeU.get_base decls in

		let rec add_type(typ: Ast.typ): unit = (*TODO:NAME*)
			match typ with
			| Ast.TypeAccess(Ast.Access(loc, name) as access) ->
				add access (ScopeU.get base_scope loc name)
			| Ast.TypeFn(_, return_type, parameter_types) ->
				add_type return_type;
				ArrayU.iter parameter_types add_type in

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

		let bind_signature(Ast.Signature(_, return_type, params)): unit =
			add_type return_type;
			ArrayU.iter params @@ fun (Ast.Parameter(_, _, typ)) -> add_type typ in

		ArrayU.iter decls begin function
		| Ast.DeclFn(Ast.Fn(_, _, (Ast.Signature(_, _, params) as signature), body)) ->
			bind_signature signature;
			bind_expr (ScopeU.add_params base_scope params) body

		| Ast.DeclRc(Ast.Rc(_, _, properties)) ->
			ArrayU.iter properties @@ fun (Ast.Property(_, _, typ)) -> add_type typ

		| Ast.DeclUn(Ast.Un(_, _, types)) ->
			ArrayU.iter types add_type

		| Ast.DeclFt(Ast.Ft(_, _, signature)) ->
			bind_signature signature
		end
	end

let output accesses =
	AstU.AccessLookup.output AstU.output_access BindingU.output accesses
