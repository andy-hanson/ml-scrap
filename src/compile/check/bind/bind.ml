type t = Binding.t AstU.AccessLookup.t
let binding = AstU.AccessLookup.get

let bind(Ast.Modul(decls)): t =
	AstU.AccessLookup.build begin fun add ->
		let base_scope = ScopeU.get_base decls in

		let add_type(typ: Ast.typ): unit = (*TODO:NAME*)
			match typ with
			| Ast.TypeAccess((loc, name) as access) ->
				add access (ScopeU.get base_scope loc name) in

		let rec bind_cases(scope: Scope.t)(cases: Ast.case_part array): unit =
			ArrayU.iter cases begin fun (_, test, result) ->
				match test with
				| Ast.AsTest(_, declare, typ) ->
					add_type typ;
					let scope = ScopeU.add_local scope declare in
					bind_expr scope result
			end

		and bind_expr(scope: Scope.t)(expr: Ast.expr): unit =
			match expr with
			| Ast.ExprAccess((loc, name) as access) ->
				add access @@ ScopeU.get scope loc name
			| Ast.Call(_, called, arguments) ->
				bind_expr scope called;
				ArrayU.iter arguments @@ bind_expr scope
			| Ast.Case(_, cased, cases) ->
				bind_expr scope cased;
				bind_cases scope cases
			| Ast.Let(_, declare, value, expr) ->
				bind_expr scope value;
				let scope = ScopeU.add_local scope declare in
				bind_expr scope expr
			| Ast.Literal _ ->
				()
			| Ast.Seq(_, a, b) ->
				bind_expr scope a;
				bind_expr scope b in

		let bind_signature((_, return_type, parameters): Ast.signature): unit =
			add_type return_type;
			ArrayU.iter parameters @@ fun (_, _, typ) -> add_type typ in

		ArrayU.iter decls begin function
		| Ast.DeclFn((_, _, ((_, _, params) as signature), body)) ->
			bind_signature signature;
			bind_expr (ScopeU.add_params base_scope params) body

		| Ast.DeclCn((_, _, typ, cases)) ->
			add_type typ;
			bind_cases base_scope cases

		| Ast.DeclRt((_, _, properties)) ->
			ArrayU.iter properties @@ fun (_, _, typ) -> add_type typ

		| Ast.DeclUn((_, _, types)) ->
			ArrayU.iter types add_type

		| Ast.DeclFt((_, _, signature)) ->
			bind_signature signature

		| Ast.DeclCt((_, _, cases)) ->
			ArrayU.iter cases @@ fun (return, input) -> add_type return; add_type input
		end
	end

let output accesses =
	AstU.AccessLookup.output AstU.output_access BindingU.output accesses
