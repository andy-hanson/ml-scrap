module ExprTypes = AstU.ExprLookup
type expr_types = Type.t ExprTypes.t
module Fts = AstU.FnLookup
type fts = Type.ft Fts.t
module LocalTypes = AstU.LocalDeclareLookup
type local_types = Type.t LocalTypes.t
module ParameterTypes = AstU.ParameterLookup
type parameter_types = Type.t ParameterTypes.t

type t = {fts: fts; parameter_types: parameter_types; expr_types: expr_types; local_types: local_types}
let type_of_expr({expr_types; _}: t) = ExprTypes.get expr_types
let type_of_fn({fts; _}: t) = Fts.get fts
let type_of_local({local_types; _}: t) = LocalTypes.get local_types
let type_of_parameter({parameter_types; _}: t) = ParameterTypes.get parameter_types

let build_fts(declared_type: Ast.typ -> Type.t)(fn_asts: Ast.fn array): fts * parameter_types =
	let parameter_types: parameter_types = ParameterTypes.create() in
	let fts: fts = Fts.build_from_keys fn_asts begin fun (Ast.Fn(_, name, Ast.Signature(_, return_type_ast, params), _)) ->
		let return_type = declared_type return_type_ast in
		let parameters = ArrayU.map params begin fun (Ast.Parameter(_, name, typ) as parameter) ->
			name, U.returning (declared_type typ) (ParameterTypes.set parameter_types parameter)
		end in
		TypeU.ft name return_type parameters
	end in
	fts, parameter_types

let f(binding: Ast.access -> Binding.t)(type_of_ast: TypeOfAst.t)(fn_asts: Ast.fn array): t =
	let declared_type = TypeCheckU.declared_type binding type_of_ast in
	let fts, parameter_types = build_fts declared_type fn_asts in

	let expr_types: expr_types = ExprTypes.create() in
	let local_types: local_types = LocalTypes.create() in
	let get_local_type = LocalTypes.get local_types in
	let set_local_type = LocalTypes.set local_types in

	let assert_assignable(a: Type.t)(b: Type.t)(loc: Loc.t): unit =
		CompileErrorU.check (TypeU.is_subtype a b) loc (CompileError.NotExpectedType(a, b)) in

	let rec assert_type(expected: Type.t)(expr: Ast.expr): unit =
		let actual = check_expr expr in
		assert_assignable expected actual (AstU.expr_loc expr)

	and check_expr(expr: Ast.expr): Type.t =
		let expr_type = match expr with
			| Ast.ExprAccess(Ast.Access(_, _) as access) ->
				begin match binding access  with
				| Binding.Builtin {Builtin.value; _} ->
					ValU.type_of value
				| Binding.Declared d ->
					begin match d with
					| Ast.DeclFn f ->
						Type.Ft (Fts.get fts f)
					| Ast.DeclRc rc_ast ->
						(*TODO: Factor out to fn_of_rc helper somewhere in TypeU*)
						let {Type.rname; Type.properties} as rc = TypeOfAst.rc_of_ast type_of_ast rc_ast in
						let params = ArrayU.map properties (fun {Type.prop_name; Type.prop_type} -> prop_name, prop_type) in
						TypeU.t_ft rname (Type.Rc rc) params
					| Ast.DeclUn _ | Ast.DeclFt _ ->
						raise U.TODO (*TODO: compile error: not a value*)
					end
				| Binding.Local(declare) ->
					get_local_type declare
				| Binding.Parameter(Ast.Parameter(_, _, typ)) ->
					declared_type typ
				| Binding.BuiltinType _ ->
					raise U.TODO (*TODO: some appropriate error*)
				end

			| Ast.Call(loc, called, args) ->
				let called_type = check_expr called in
				let return_type, param_types = match called_type with
					| Type.Ft {Type.fname = _; Type.return_type; Type.parameters} ->
						return_type, ArrayU.map parameters (fun (_, typ) -> typ)
					| _ ->
						CompileErrorU.raise loc (CompileError.NotCallable called_type) in
				let n_params = Array.length param_types in
				let n_args = Array.length args in
				CompileErrorU.check (n_params = n_args) loc (CompileError.NumArgs(n_params, n_args));
				(*TODO: use parameter name for helpful error info*)
				ArrayU.iter_zip param_types args assert_type;
				return_type

			| Ast.Case(loc, cased, parts) ->
				let types =
					match check_expr cased with
					| Type.Un {Type.types; _} -> types
					| typ -> CompileErrorU.raise (AstU.expr_loc cased) (CompileError.CanOnlyCaseUnion typ) in
				let remaining_types, part_types =
					ArrayU.fold_map types parts begin fun remaining_types (Ast.CasePart(_, test, result)) ->
						let Ast.AsTest(test_loc, declare, test_type_ast) = test in
						let test_type = declared_type test_type_ast in
						set_local_type declare test_type;
						let remaining_types = match ArrayU.try_remove remaining_types test_type with
						| Some types -> types
						| None -> CompileErrorU.raise test_loc (CompileError.CasePartType(remaining_types, test_type)) in
						remaining_types, check_expr result
					end in
				CompileErrorU.check (ArrayU.empty remaining_types) loc (CompileError.CasesUnhandled remaining_types);
				TypeCheckU.combine_types loc part_types

			| Ast.Let(_, declare, value, expr) ->
				let typ = check_expr value in
				set_local_type declare typ;
				check_expr expr

			| Ast.Literal(_, v) ->
				ValU.type_of v

			| Ast.Seq(_, a, b) ->
				assert_type Type.Void a;
				check_expr b in

		ExprTypes.set expr_types expr expr_type;
		expr_type in

	ArrayU.iter fn_asts begin fun (Ast.Fn(_, _, _, body) as fn) ->
		let return_type = (Fts.get fts fn).Type.return_type in
		assert_type return_type body
	end;

	{fts; parameter_types; expr_types; local_types}
