(* Keys should all be Ast.Access *)
module Values = AstU.ExprLookup
type values =  Binding.t Values.t
(* Keys should all be Ast.TypeAccess *)
module Types = AstU.TypLookup
type types = Binding.t Types.t

type t = { values: values; types: types }
let value_binding {values; _} access = Values.get values access
let type_binding {types; _} access = Types.get types access

let do_bind(base_scope: Scope.t)(add_value: Ast.expr -> Binding.t -> unit)(add_type: Ast.typ -> Binding.t -> unit)(decls: Ast.decl array): unit =
	let add_type(Ast.TypeAccess(loc, name) as access): unit =
		add_type access (ScopeU.get base_scope loc name) in

	let rec bind_expr(scope: Scope.t)(expr: Ast.expr): unit =
		match expr with
		| Ast.Access(loc, name) ->
			add_value expr (ScopeU.get scope loc name)
		| Ast.Call(_, called, arguments) ->
			bind_expr scope called;
			ArrayU.iter arguments (bind_expr scope)
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
	U.returning { values = Values.create(); types = Types.create() } begin fun {values; types} ->
		let base_scope = ScopeU.get_base ctx decls in
		do_bind base_scope (Values.set values) (Types.set types) decls
	end

(* boilerplate *)

let output(out: 'o OutputU.t)({values; types}: t): unit =
	OutputU.out out "values: %a\ntypes: %a"
		(Values.output AstU.output_expr Binding.output) values
		(Types.output AstU.output_typ Binding.output) types
