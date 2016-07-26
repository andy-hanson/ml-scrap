open Ast

type t = {
	vals: Binding.v AstLookup.Access.t;
	tys: Binding.ty AstLookup.Access.t;
}
let binding({vals; _}: t) = AstLookup.Access.get vals
let ty_binding({tys; _}: t) = AstLookup.Access.get tys

type ctx = {
	scope: Scope.t;
	add_v: access -> Binding.v -> unit;
	add_ty: access -> Binding.ty -> unit;
}

let bind_ty({scope; add_ty; _}: ctx)(ty: ty): unit = (*TODO:NAME*)
	U.loop ty @@ fun loop -> function
		| TyAccess((loc, name) as access) ->
			add_ty access @@ ScopeU.get_ty scope loc name
		| TyInst(_, gen, arguments) ->
			loop gen;
			ArrayU.iter arguments loop

let rec add_pattern_to_scope(scope: Scope.t)(pattern: Ast.pattern): Scope.t =
	match pattern with
	| PSingle(declare) ->
		ScopeU.add_local scope declare
	| PDestruct(_, patterns) ->
		ArrayU.fold scope patterns add_pattern_to_scope

let rec bind_cases({scope; _} as ctx: ctx)(cases: cs_part array): unit =
	ArrayU.iter cases @@ fun (_, (_, ty, pattern), result) ->
		bind_ty ctx ty;
		bind_expr {ctx with scope = add_pattern_to_scope scope pattern} result

and bind_expr({scope; add_v; _} as ctx: ctx)(expr: expr): unit =
	let recur = bind_expr ctx in
	match expr with
	| At(_, _, ty, expr) ->
	 	bind_ty ctx ty;
		recur expr
	| ExprType ty ->
		bind_ty ctx ty
	| ExprAccess((loc, name) as access) ->
		add_v access @@ ScopeU.get_v scope loc name
	| Call(_, called, arguments) ->
		recur called;
		ArrayU.iter arguments recur
	| Cs(_, cased, cases) ->
		recur cased;
		bind_cases ctx cases
	| GetProperty(_, expr, _) ->
		recur expr
	| Let(_, pattern, value, expr) ->
		recur value;
		let scope = add_pattern_to_scope scope pattern in
		bind_expr {ctx with scope} expr
	| Literal _ ->
		()
	| Seq(_, a, b) ->
		recur a;
		recur b
	| Partial(_, f, args) ->
		recur f;
		ArrayU.iter args recur
	| Quote(_, _, parts) ->
		ArrayU.iter parts @@ fun (expr, _) -> recur expr
	| Check(_, expr) ->
		recur expr
	| GenInst(_, expr, tys) ->
		recur expr;
		ArrayU.iter tys @@ bind_ty ctx

let bind_signature(ctx: ctx)((_, return_ty, parameters): signature): unit =
	bind_ty ctx return_ty;
	ArrayU.iter parameters @@ fun (_, _, ty) -> bind_ty ctx ty

let bind_properties(ctx: ctx)(properties: Ast.property array): unit =
	ArrayU.iter properties @@ fun (_, _, ty) -> bind_ty ctx ty

let bind(get_modul: Path.rel -> N.Compiler.modul)((imports, decls): modul): t =
	let vals = AstLookup.Access.create() in
	let tys = AstLookup.Access.create() in

	U.returning {vals; tys} @@ fun _ ->
		let base_scope = ScopeU.get_base get_modul imports decls in
		let ctx = {scope = base_scope; add_v = AstLookup.Access.set vals; add_ty = AstLookup.Access.set tys} in

		ArrayU.iter decls @@ function
			| DeclVal v ->
				begin match v with
				| Fn(_, head, ((_, _, params) as signature), body) ->
					let ctx_with_ty_params =
						match head with
						| Ast.FnPlain _ -> ctx
						| Ast.FnGeneric(_, params) -> {ctx with scope = ScopeU.add_ty_params ctx.scope params} in
					bind_signature ctx_with_ty_params signature;
					let scope = ScopeU.add_params base_scope params in
					bind_expr {ctx with scope} body
				end
			| DeclTy t ->
				begin match t with
				| Rt(_, _, properties) ->
					bind_properties ctx properties
				| GenRt(_, _, ty_parameters, properties) ->
					let scope = ScopeU.add_ty_params base_scope ty_parameters in
					bind_properties {ctx with scope} properties
				| Un(_, _, tys) ->
					ArrayU.iter tys @@ bind_ty ctx
				| Ft(_, _, signature) ->
					bind_signature ctx signature
				end

let output(out: 'o OutputU.t)({vals; tys}: t): unit =
	OutputU.out out "Bind(%a, %a)"
		(AstLookup.Access.output AstOut.output_access BindingU.output_v) vals
		(AstLookup.Access.output AstOut.output_access BindingU.output_ty) tys
