open Ast

type t = {
	vals: Binding.v AstU.AccessLookup.t;
	tys: Binding.ty AstU.AccessLookup.t;
}
let binding({vals; _}: t) = AstU.AccessLookup.get vals
let ty_binding({tys; _}: t) = AstU.AccessLookup.get tys

type ctx = {
	scope: Scope.t;
	add_v: access -> Binding.v -> unit;
	add_ty: access -> Binding.ty -> unit;
}

(*TODO: should be called bind_ty*)
let add_ty({scope; add_ty; _}: ctx)(ty: ty): unit = (*TODO:NAME*)
	let rec recur = function
		| TyAccess((loc, name) as access) ->
			add_ty access @@ ScopeU.get_ty scope loc name
		| TyInst(_, gen, arguments) ->
			recur gen;
			ArrayU.iter arguments recur in
	recur ty

let rec add_pattern_to_scope(scope: Scope.t)(pattern: Ast.pattern): Scope.t =
	match pattern with
	| PSingle(declare) ->
		ScopeU.add_local scope declare
	| PDestruct(_, patterns) ->
		ArrayU.fold scope patterns add_pattern_to_scope

let rec bind_cases({scope; _} as ctx: ctx)(cases: cs_part array): unit =
	ArrayU.iter cases begin fun (_, (_, ty, pattern), result) ->
		add_ty ctx ty;
		bind_expr {ctx with scope = add_pattern_to_scope scope pattern} result
	end

and bind_expr({scope; add_v; _} as ctx: ctx)(expr: expr): unit =
	let recur = bind_expr ctx in
	match expr with
	| At(_, _, ty, expr) ->
	 	add_ty ctx ty;
		recur expr
	| ExprType ty ->
		add_ty ctx ty
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
		ArrayU.iter tys @@ add_ty ctx

let bind((_, decls): modul): t =
	let vals = AstU.AccessLookup.create() in
	let tys = AstU.AccessLookup.create() in

	U.returning {vals; tys} begin fun _ ->
		let base_scope = ScopeU.get_base decls in
		let ctx = {scope = base_scope; add_v = AstU.AccessLookup.set vals; add_ty = AstU.AccessLookup.set tys} in

		let bind_signature((_, return_ty, parameters): signature): unit =
			add_ty ctx return_ty;
			ArrayU.iter parameters @@ fun (_, _, ty) -> add_ty ctx ty in

		ArrayU.iter decls begin function
		| DeclVal v ->
			begin match v with
			| Fn((_, _, ((_, _, params) as signature), body)) ->
				bind_signature signature;
				let scope = ScopeU.add_params base_scope params in
				bind_expr {ctx with scope} body
			end
		| DeclTy t ->
			begin match t with
			| Rt((_, _, properties)) ->
				ArrayU.iter properties @@ fun (_, _, ty) -> add_ty ctx ty
			| Un((_, _, tys)) ->
				ArrayU.iter tys @@ add_ty ctx
			| Ft((_, _, signature)) ->
				bind_signature signature
			end
		end
	end

let output(out: 'o OutputU.t)({vals; tys}: t): unit =
	OutputU.out out "Bind(%a, %a)"
		(AstU.AccessLookup.output AstU.output_access BindingU.output_v) vals
		(AstU.AccessLookup.output AstU.output_access BindingU.output_ty) tys
