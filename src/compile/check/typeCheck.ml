module ExprTypes = AstU.ExprLookup
type expr_types = Type.t ExprTypes.t
module FnTypes = AstU.FnLookup
type fn_types = Type.fn FnTypes.t
module Rcs = AstU.RcLookup
type rcs = Type.rc Rcs.t
module LocalTypes = AstU.LocalDeclareLookup
type local_types = Type.t LocalTypes.t

type t = { expr_types: expr_types; fn_types: fn_types; local_types: local_types; rcs: rcs }
let type_of_expr({expr_types; _}: t) = ExprTypes.get expr_types
let type_of_fn({fn_types; _}: t) = FnTypes.get fn_types
let type_of_local({local_types; _}: t) = LocalTypes.get local_types
let rc_of_ast({rcs; _}: t) = Rcs.get rcs
let all_rcs({rcs; _}: t): Type.rc array = Rcs.values rcs

(*TODO:MOVE*)
let combine_types(loc: Loc.t)(types: Type.t array): Type.t =
	(*TODO: actual union algorithm*)
	let t = Array.get types 0 in
	ArrayU.iter types begin fun typ ->
		CompileErrorU.check (t = typ) loc (CompileError.CombineTypes(t, typ))
	end;
	t

let rec declared_type(bindings: Bind.t)(get_rc: Ast.rc -> Type.rc)(typ: Ast.typ): Type.t =
	match typ with
	| Ast.Or(_, parts) ->
		Type.Or(ArrayU.map parts (declared_type bindings get_rc))
	| Ast.TypeAccess(Ast.Access(_, _) as access) ->
		(*TODO: helper*)
		match Bind.binding bindings access with
		| Binding.Builtin _ | Binding.Local _ | Binding.Parameter _ ->
			raise U.TODO (*TODO: not-a-type error*)
		| Binding.BuiltinType b ->
			b
		| Binding.Declared d ->
			begin match d with
			| Ast.DeclRc r ->
				Type.Rc (get_rc r)
			| _ ->
				assert false
			end

let build_rcs(bindings: Bind.t)(rc_asts: Ast.rc array): rcs =
	let rcs = Rcs.build_from_keys rc_asts begin fun (Ast.Rc(_, name, _)) ->
		{ Type.rname = name; Type.properties = [||] }
	end in
	U.returning rcs begin fun _ ->
		Rcs.iter rcs begin fun (Ast.Rc(_, _, properties)) rc ->
			rc.Type.properties <- ArrayU.map properties begin fun (Ast.Property(_, name, typ)) ->
				{ Type.prop_name = name; Type.prop_type = declared_type bindings (Rcs.get rcs) typ }
			end
		end
	end

let build_fn_types(declared_type: Ast.typ -> Type.t)(fn_asts: Ast.fn array): fn_types =
	FnTypes.build_from_keys fn_asts begin fun (Ast.Fn(_, _, Ast.Signature(_, return_type_ast, params), _)) ->
		let return_type = declared_type return_type_ast in
		let param_types = ArrayU.map params (fun (Ast.Parameter(_, _, typ)) -> declared_type typ) in
		TypeU.fn return_type param_types
	end

let f(Ast.Modul(decls))(bindings: Bind.t): t =
	let fn_asts, rc_asts = AstU.modul_split decls in
	let rcs = build_rcs bindings rc_asts in
	let declared_type = declared_type bindings (Rcs.get rcs) in
	let fn_types = build_fn_types declared_type fn_asts in

	let expr_types: expr_types = ExprTypes.create() in
	let local_types: local_types = LocalTypes.create() in

	let assert_assignable(a: Type.t)(b: Type.t)(loc: Loc.t): unit =
		CompileErrorU.check (a = b) loc (CompileError.NotExpectedType(a, b)) in

	let rec assert_type(expected: Type.t)(expr: Ast.expr): unit =
		let actual = check_expr expr in
		assert_assignable expected actual (AstU.expr_loc expr)

	and check_case_part(type_should_handle: Type.t)(Ast.CasePart(_, test, result)): Type.t =
		let Ast.AsTest(test_loc, declare, test_type_ast) = test in
		let test_type = declared_type test_type_ast in
		CompileErrorU.check (type_should_handle = test_type) test_loc (CompileError.CasePartType(type_should_handle, test_type));
		(*TODO: set_local_type helper*)
		LocalTypes.set local_types declare test_type;
		check_expr result

	and check_expr(expr: Ast.expr): Type.t =
		let expr_type = match expr with
			| Ast.ExprAccess(Ast.Access(_, _) as access) ->
				begin match Bind.binding bindings access  with
				| Binding.Builtin b ->
					BuiltinU.type_of b
				| Binding.Declared d ->
					begin match d with
					| Ast.DeclFn f ->
						Type.Fn (FnTypes.get fn_types f)
					| Ast.DeclRc rc_ast ->
						let rc = Rcs.get rcs rc_ast in
						let params = ArrayU.map rc.Type.properties (fun p -> p.Type.prop_type) in
						TypeU.t_fn (Type.Rc rc) params
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
						CompileErrorU.raise loc (CompileError.NotCallable called_type) in
				let n_params = Array.length param_types in
				let n_args = Array.length args in
				CompileErrorU.check (n_params = n_args) loc (CompileError.NumArgs(n_params, n_args));
				ArrayU.iter_zip param_types args assert_type;
				return_type

			| Ast.Case(loc, cased, parts) ->
				(*
				Da rulez:
				cased must be a union type
				parts much be each member of the union (currently, in order)
				*)
				let types =
					match check_expr cased with
					| Type.Or parts ->
						parts
					| x ->
						CompileErrorU.raise (AstU.expr_loc cased) (CompileError.CanOnlyCaseUnion x) in
				CompileErrorU.check (ArrayU.same_length parts types) loc (CompileError.CaseLength(types, (Array.length parts)));
				combine_types loc (ArrayU.map_zip types parts begin fun typ part ->
					check_case_part typ part
				end)

			| Ast.Let(_, declare, value, expr) ->
				let typ = check_expr value in
				LocalTypes.set local_types declare typ;
				check_expr expr

			| Ast.Literal(_, v) ->
				ValU.typ v

			| Ast.Seq(_, a, b) ->
				assert_type Type.Void a;
				check_expr b in

		ExprTypes.set expr_types expr expr_type;
		expr_type in

	ArrayU.iter fn_asts begin fun (Ast.Fn(_, _, _, body) as fn) ->
		let return_type = (FnTypes.get fn_types fn).Type.return_type in
		assert_type return_type body
	end;

	{ expr_types; local_types; fn_types; rcs }
