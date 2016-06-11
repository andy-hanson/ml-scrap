module ExprTypes = AstU.ExprLookup
type expr_types = Type.t ExprTypes.t
module FnTypes = AstU.FnLookup
type fn_types = Type.fn FnTypes.t
module Rcs = AstU.RcLookup
type rcs = Type.rc Rcs.t

type t = { expr_types: expr_types; fn_types: fn_types; rcs: rcs }
let type_of_expr({expr_types; _}: t) = ExprTypes.get expr_types
let type_of_fn({fn_types; _}: t) = FnTypes.get fn_types
let rc_of_ast({rcs; _}: t) = Rcs.get rcs
let all_rcs({rcs; _}: t): Type.rc array = Rcs.values rcs

module LocalTypes = AstU.LocalDeclareLookup
type local_types = Type.t LocalTypes.t

let declared_type(bindings: Bind.t)(rcs: rcs)(typ: Ast.typ): Type.t =
	match Bind.type_binding bindings typ with
	| Binding.Builtin _ | Binding.Local _ | Binding.Parameter _ ->
		raise U.TODO (*TODO: not-a-type error*)
	| Binding.BuiltinType b ->
		b
	| Binding.Declared d ->
		begin match d with
		| Ast.DeclRc r ->
			Type.Rc (Rcs.get rcs r)
		| _ ->
			raise U.TODO (*TODO: not-a-type error*)
		end

let build_rcs(bindings: Bind.t)(decls: Ast.decl array): rcs =
	let rc_asts = AstU.modul_rcs decls in
	let rcs = Rcs.build_from_keys rc_asts begin fun (Ast.Rc(_, name, _)) ->
		{ Type.rname = name; Type.properties = [||] }
	end in
	ArrayU.iter rc_asts begin fun (Ast.Rc(_, _, properties) as rc_ast) ->
		let rc = Rcs.get rcs rc_ast in
		rc.Type.properties <- ArrayU.map properties begin fun (Ast.Property(_, name, typ)) ->
			{ Type.prop_name = name; Type.prop_type = declared_type bindings rcs typ }
		end
	end;
	rcs

let f(Ast.Modul(decls))(bindings: Bind.t): t =
	let rcs = build_rcs bindings decls in
	let declared_type = declared_type bindings rcs in

	let expr_types: expr_types = ExprTypes.create() in
	let local_types: local_types = LocalTypes.create() in

	let fn_type(Ast.Fn(_, _, Ast.Signature(_, return_type_ast, params), _)): Type.fn =
		let param_types = ArrayU.map params begin fun (Ast.Parameter(_, _, typ)) ->
			declared_type typ
		end in
		let return_type = declared_type return_type_ast in
		Type.fn return_type param_types in

	let fn_types = FnTypes.create() in
	(*TODO:neater*)
	ArrayU.iter decls begin function
		| Ast.DeclFn f ->
			FnTypes.set fn_types f (fn_type f)
		| Ast.DeclRc _ -> ()
	end;

	let assert_assignable(a: Type.t)(b: Type.t)(loc: Loc.t): unit =
		CompileError.check (a = b) loc (CompileError.NotExpectedType(a, b)) in

	let rec assert_type(expected: Type.t)(expr: Ast.expr): unit =
		let actual = check_expr expr in
		assert_assignable expected actual (Ast.expr_loc expr)

	and check_expr(expr: Ast.expr): Type.t =
		let expr_type = match expr with
			| Ast.Access(_, _) ->
				begin match Bind.value_binding bindings expr  with
				| Binding.Builtin b ->
					Builtins.type_of b
				| Binding.Declared d ->
					begin match d with
					| Ast.DeclFn f ->
						Type.Fn (FnTypes.get fn_types f)
					| Ast.DeclRc rc_ast ->
						let rc = Rcs.get rcs rc_ast in
						let params = ArrayU.map rc.Type.properties (fun p -> p.Type.prop_type) in
						Type.t_fn (Type.Rc rc) params
					end
				| Binding.Local(declare) ->
					LocalTypes.get local_types declare
				| Binding.Parameter(Ast.Parameter(_, _, typ)) ->
					declared_type typ
				| Binding.BuiltinType _ ->
					raise U.TODO (*TODO: some appropriate error*)
				end

			| Ast.Call(loc, called, args) ->
				let called_type = check_expr called in
				let return_type, param_types = match called_type with
					| Type.Fn {Type.return_type; Type.parameters} ->
						return_type, parameters
					| _ ->
						CompileError.raise loc (CompileError.NotCallable called_type) in
				let n_params = Array.length param_types in
				let n_args = Array.length args in
				CompileError.check (n_params = n_args) loc (CompileError.NumArgs(n_params, n_args));
				ArrayU.iter_zip param_types args begin fun typ arg ->
					assert_type typ arg
				end;
				return_type

			| Ast.Let(_, declare, value, expr) ->
				let typ = check_expr value in
				LocalTypes.set local_types declare typ;
				check_expr expr

			| Ast.Literal(_, v) ->
				Val.typ v

			| Ast.Seq(_, a, b) ->
				assert_type Type.Void a;
				check_expr b in

		ExprTypes.set expr_types expr expr_type;
		expr_type in

	(*TODO: we iterate too many times!*)
	ArrayU.iter decls begin function
		| Ast.DeclFn(Ast.Fn(_, _, _, body) as fn) ->
			(*TODO:NAME*)
			let foo: Type.fn = FnTypes.get fn_types fn in
			let t: Type.t = foo.Type.return_type in
			assert_type t body
		| Ast.DeclRc _ ->
			(*TODO: only iterate over module once, and split*)
			()
	end;

	{ expr_types; fn_types; rcs }
