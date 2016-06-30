type t = Binding.t AstU.AccessLookup.t
let binding = AstU.AccessLookup.get

(*TODO: cleanup*)
let bind(decls: Ast.modul): t =
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
			let recur = bind_expr scope in
			match expr with
			| Ast.ExprAccess((loc, name) as access) ->
				add access @@ ScopeU.get scope loc name
			| Ast.Call(_, called, arguments) ->
				recur called;
				ArrayU.iter arguments recur
			| Ast.Case(_, cased, cases) ->
				recur cased;
				bind_cases scope cases
			| Ast.Let(_, declare, value, expr) ->
				recur value;
				let scope = ScopeU.add_local scope declare in
				bind_expr scope expr
			| Ast.Literal _ ->
				()
			| Ast.Seq(_, a, b) ->
				recur a;
				recur b
			| Ast.Partial(_, f, args) ->
			 	recur f;
				ArrayU.iter args recur
			| Ast.Quote(_, _, parts) ->
				ArrayU.iter parts @@ fun (expr, _) -> recur expr in

		let bind_signature((_, return_type, parameters): Ast.signature): unit =
			add_type return_type;
			ArrayU.iter parameters @@ fun (_, _, typ) -> add_type typ in

		ArrayU.iter decls begin function
		| Ast.Fn((_, _, ((_, _, params) as signature), body)) ->
			bind_signature signature;
			bind_expr (ScopeU.add_params base_scope params) body

		| Ast.Cn((_, _, typ, cases)) ->
			add_type typ;
			bind_cases base_scope cases

		| Ast.Rt((_, _, properties)) ->
			ArrayU.iter properties @@ fun (_, _, typ) -> add_type typ

		| Ast.Un((_, _, types)) ->
			ArrayU.iter types add_type

		| Ast.Ft((_, _, signature)) ->
			bind_signature signature

		| Ast.Ct((_, _, cases)) ->
			ArrayU.iter cases @@ fun (return, input) -> add_type return; add_type input
		end
	end

let output accesses =
	AstU.AccessLookup.output AstU.output_access BindingU.output accesses
