module ExprTypes = AstU.ExprLookup
type expr_types = N.ty ExprTypes.t
module LocalTypes = AstU.LocalDeclareLookup
type local_types = N.ty LocalTypes.t

type t = {expr_types: expr_types; local_types: local_types}
let type_of_expr({expr_types; _}: t) = ExprTypes.get expr_types
let type_of_local({local_types; _}: t) = LocalTypes.get local_types

(*type decls have already been checked in typeofast*)
let f(binding: Ast.access -> Binding.t)(type_of_ast: TypeOfAst.t)(Ast.Modul(decls)): t =
	let declared_type = TypeOfAst.declared_type binding type_of_ast in
	(*let fts, parameter_types = build_fts declared_type fn_asts in*)

	let expr_types: expr_types = ExprTypes.create() in
	let local_types: local_types = LocalTypes.create() in
	let get_local_type = LocalTypes.get local_types in
	let set_local_type = LocalTypes.set local_types in

	(*TODO:MOVE?*)
	let ft_of_fn(fn: Ast.fn): N.ft =
		let {N.fn_type; _} = TypeOfAst.fn_of_ast type_of_ast fn in
		N.fn_type_as_ft fn_type in

	let assert_assignable(a: N.ty)(b: N.ty)(loc: Loc.t): unit =
		CompileErrorU.check (TypeU.is_subtype a b) loc @@ CompileError.NotExpectedType(a, b) in

	let rec assert_type(expected: N.ty)(expr: Ast.expr): unit =
		let actual = check_expr expr in
		assert_assignable expected actual @@ AstU.expr_loc expr

	and check_expr(expr: Ast.expr): N.ty =
		let expr_type = match expr with
			| Ast.ExprAccess((loc, _) as access) ->
				begin match binding access  with
				| Binding.Builtin {Builtin.value; _} ->
					ValU.type_of value
				| Binding.Declared d ->
					(*TODO: type-as-value helper could be useful here*)
					begin match d with
					| Ast.DeclFn fn ->
						N.Ft(ft_of_fn fn)
					| Ast.DeclCn(_, _, typ, _) ->
						declared_type typ
					| Ast.DeclRt rt_ast ->
						(*TODO: Factor out to fn_of_rc helper somewhere in TypeU*)
						let {N.rname; N.properties} as rt = TypeOfAst.rt_of_ast type_of_ast rt_ast in
						TypeU.t_ft rname (N.Rt rt) properties
					| Ast.DeclUn _ | Ast.DeclFt _ | Ast.DeclCt _ ->
						raise U.TODO (*TODO: compile error: not a value*)
					end
				| Binding.Local declare ->
					get_local_type declare
				| Binding.Parameter parameter ->
					TypeOfAst.parameter_type type_of_ast parameter
				| Binding.BuiltinType t ->
					(*TODO: type-as-value helper*)
					begin match t with
					| N.Rt({N.rname; N.properties} as rt) ->
						(*TODO: duplicate code of above*)
						TypeU.t_ft rname (N.Rt rt) properties
					| _ ->
						CompileErrorU.raise loc @@ CompileError.NotAValue access
					end
				end

			| Ast.Call(loc, called, args) ->
				let called_type = check_expr called in
				begin match called_type with
				| N.Ft {N.fname = _; N.return_type; N.parameters} ->
					let param_types = ArrayU.map parameters (fun (_, typ) -> typ) in
					let n_params = Array.length param_types in
					let n_args = Array.length args in
					CompileErrorU.check (n_params = n_args) loc @@ CompileError.NumArgs(n_params, n_args);
					(*TODO: use parameter name for helpful error info*)
					ArrayU.iter_zip param_types args assert_type;
					return_type
				| N.Ct {N.cname = _; N.ct_cases} ->
					CompileErrorU.check (Array.length args = 1) loc @@ CompileError.NumArgs(1, Array.length args);
					let arg = ArrayU.head args in
					let arg_type = check_expr arg in
					ArrayU.find_map ct_cases begin fun (return, input) ->
						OpU.op_if (TypeU.is_subtype input arg_type) @@ fun () -> return
					end
				| _ ->
					CompileErrorU.raise loc @@ CompileError.NotAFunction called_type
				end

			| Ast.Case(loc, cased, parts) ->
				check_case loc cased parts

			| Ast.Let(_, declare, value, expr) ->
				let typ = check_expr value in
				set_local_type declare typ;
				check_expr expr

			| Ast.Literal(_, v) ->
				ValU.type_of v

			| Ast.Seq(_, a, b) ->
				assert_type N.TVoid a;
				check_expr b in

		ExprTypes.set expr_types expr expr_type;
		expr_type

	and check_case(loc: Loc.t)(cased: Ast.expr)(parts: Ast.case_part array): N.ty =
		let cased_types =
			match check_expr cased with
			| N.Un {N.utypes; _} -> utypes
			| t -> CompileErrorU.raise (AstU.expr_loc cased) (CompileError.CanOnlyCaseUnion t) in
		let remaining_types, part_types =
			ArrayU.fold_map cased_types parts begin fun remaining_types (_, test, result) ->
				let Ast.AsTest(test_loc, declare, test_type_ast) = test in
				let test_type = declared_type test_type_ast in
				set_local_type declare test_type;
				let remaining_types = match ArrayU.try_remove remaining_types test_type with
				| Some types -> types
				| None -> CompileErrorU.raise test_loc @@ CompileError.CasePartType(remaining_types, test_type) in
				remaining_types, check_expr result
			end in
		CompileErrorU.check (ArrayU.empty remaining_types) loc @@ CompileError.CasesUnhandled remaining_types;
		TypeCheckU.join loc part_types

	(*TODO: share code with check_case*)
	and check_cn(_loc: Loc.t)(cases: N.ct_case array)(parts: Ast.case_part array): unit =
		let remaining_cases = ArrayU.fold cases parts begin fun remaining_cases (_, test, result) ->
			let Ast.AsTest(_test_loc, declare, test_type_ast) = test in
			let test_type = declared_type test_type_ast in
			set_local_type declare test_type;
			let (_, return), remaining_cases =
				match ArrayU.try_remove_where remaining_cases @@ fun (input, _) -> TypeCheckU.eq input test_type with
				| Some(x) -> x
				| None -> raise U.TODO (*TODO: CasePartType-like error*) in
			assert_type return result;
			remaining_cases
		end in
		(*TODO: CompileErrorU.check (ArrayU.empty remaining_cases) loc @@ CompileError.CasesUnhandled remaining_cases*)
		assert (ArrayU.empty remaining_cases) in

	ArrayU.iter decls begin function
		| Ast.DeclFn((_, _, _, body) as fn) ->
			let return_type = (ft_of_fn fn).N.return_type in
			assert_type return_type body
		| Ast.DeclCn((loc, _, type_ast, parts) as _cn) ->
			let typ = declared_type type_ast in
			let case_types =
				match typ with
				| N.Ct {N.ct_cases; _} -> ct_cases
				| _ -> raise U.TODO (*TODO: an appropriate error*) in
			check_cn loc case_types parts
		| Ast.DeclRt _ | Ast.DeclUn _ | Ast.DeclFt _ | Ast.DeclCt _ ->
			()
	end;

	{expr_types; local_types}
