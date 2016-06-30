(*TODO: open N*)

module ExprTypes = AstU.ExprLookup
type expr_types = N.ty ExprTypes.t
module LocalTypes = AstU.LocalDeclareLookup
type local_types = N.ty LocalTypes.t

type t = {expr_types: expr_types; local_types: local_types}
let type_of_expr({expr_types; _}: t) = ExprTypes.get expr_types
let type_of_local({local_types; _}: t) = LocalTypes.get local_types

(*type decls have already been checked in typeofast*)
let f(binding: Ast.access -> Binding.t)(type_of_ast: TypeOfAst.t)(decls: Ast.modul): t =
	let declared_type = TypeOfAst.declared_type binding type_of_ast in
	(*let fts, parameter_types = build_fts declared_type fn_asts in*)

	let expr_types: expr_types = ExprTypes.create() in
	let local_types: local_types = LocalTypes.create() in
	let get_local_type = LocalTypes.get local_types in
	let set_local_type = LocalTypes.set local_types in

	(*TODO:MOVE*)
	let ft_of_fn(fn: Ast.fn): N.ft =
		let {N.fn_type; _} = TypeOfAst.fn_of_ast type_of_ast fn in
		match fn_type with
		| N.Ft ft -> ft
		| N.Ct _ -> assert false in

	let rec assert_value_assignable(expected: N.ty)(expr: Ast.expr): unit =
		let actual = check_expr expr in
		TypeCheckU.assert_value_assignable (AstU.expr_loc expr) expected actual

	(* Expr must be a *subtype* of the expected type *)
	and assert_parameter_assignable((_, expected): N.parameter)(expr: Ast.expr): unit =
		let actual = check_expr expr in
		TypeCheckU.assert_parameter_assignable (AstU.expr_loc expr) expected actual

	and check_expr(expr: Ast.expr): N.ty =
		(*TODO: factor out some code -- this function is just too long!*)
		let expr_type =
			match expr with
			| Ast.ExprAccess((loc, _) as access) ->
				begin match binding access with
				| Binding.Builtin value ->
					ValU.type_of value
				| Binding.Declared d ->
					(*TODO: type-as-value helper could be useful here*)
					begin match d with
					| Ast.Fn fn ->
						N.TFn(N.Ft(ft_of_fn fn))
					| Ast.Cn(_, _, typ, _) ->
						declared_type typ
					| Ast.Rt rt_ast ->
						(*TODO: Factor out to fn_of_rc helper somewhere in TypeU*)
						let {N.rname; N.properties} as rt = TypeOfAst.rt_of_ast type_of_ast rt_ast in
						TypeU.t_ft rname (N.Rt rt) properties
					| Ast.Un _ | Ast.Ft _ | Ast.Ct _ ->
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
				let n_args = Array.length args in
				let cast: N.ty_fn option =
					match called with
					| Ast.ExprAccess(access) ->
						begin match binding access with
						| Binding.BuiltinType ty ->
							begin match ty with
							| N.TFn f -> Some f
							| _ -> None
							end
						| Binding.Declared d ->
							begin match d with
							| Ast.Ft ft ->
								Some(N.Ft(TypeOfAst.ft_of_ast type_of_ast ft))
							| Ast.Ct ct ->
								Some(N.Ct(TypeOfAst.ct_of_ast type_of_ast ct))
							| _ ->
								None
							end
						| _ ->
							None
						end
					| _ -> None in
				begin match cast with
				| Some cast_to ->
					U.returning (N.TFn cast_to) begin fun _ ->
						(*TODO: have specialized error message: cast takes only 1 parameter*)
						CompileErrorU.check (Array.length args = 1) loc @@ CompileError.NumArgs(1, n_args);
						let arg = args.(0) in
						TypeCheckU.assert_fn_upcast loc cast_to @@ check_expr arg
					end
				| None ->
					let called_type = check_expr called in
					begin match called_type with
					| N.TFn f ->
						begin match f with
						| N.Ft {N.fname = _; N.return_type; N.parameters} ->
							let n_params = Array.length parameters in
							CompileErrorU.check (n_params = n_args) loc @@ CompileError.NumArgs(n_params, n_args);
							(*TODO: use parameter name for helpful error info*)
							ArrayU.iter_zip parameters args assert_parameter_assignable;
							return_type
						| N.Ct {N.cname = _; N.ct_cases} ->
							CompileErrorU.check (Array.length args = 1) loc @@ CompileError.NumArgs(1, Array.length args);
							let arg = ArrayU.head args in
							let arg_type = check_expr arg in
							let found = ArrayU.find_map ct_cases begin fun (return, input) ->
								OpU.op_if (TypeCheckU.eq input arg_type) @@ fun () -> return
							end in
							OpU.or_else found @@ fun () -> raise U.TODO(*TODO: appropriate error*)
						end
					| _ ->
						CompileErrorU.raise loc @@ CompileError.NotAFunction called_type
					end
				end

			| Ast.Case(loc, cased, parts) ->
				check_case loc cased parts

			| Ast.Let(_, declare, value, expr) ->
				let typ = check_expr value in
				set_local_type declare typ;
				check_expr expr

			| Ast.Literal(_, v) ->
				N.TPrimitive(ValU.type_of_primitive v)

			| Ast.Seq(_, a, b) ->
				assert_value_assignable N.t_void a;
				check_expr b

			| Ast.Partial(_, fn, args) ->
				let fn_typ = check_expr fn in
				begin match fn_typ with
				| N.TFn f ->
					begin N.TFn begin N.Ft begin match f with
					| N.Ft {N.fname; N.return_type; N.parameters} ->
						assert (Array.length parameters >= Array.length args); (*TODO: proper error message*)
						let remaining_parameters = ArrayU.partial parameters args assert_parameter_assignable in
						(*
						TODO: it should have a different name, or some indication that it's partial now...
						(TODO: don't just manipulate strings! replace `fname` with structured data tracing the type's origin.
						*)
						{N.fname; N.return_type; N.parameters = remaining_parameters}
					| N.Ct _ ->
						raise U.TODO
					end end end
				| _ ->
					raise U.TODO (*TODO: error message*)
				end

			| Ast.Quote(_, _, parts) ->
				ArrayU.iter parts begin fun (interpolated, _) ->
					assert_value_assignable N.Any interpolated
				end;
				N.t_string in

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
			assert_value_assignable return result;
			remaining_cases
		end in
		(*TODO: CompileErrorU.check (ArrayU.empty remaining_cases) loc @@ CompileError.CasesUnhandled remaining_cases*)
		assert (ArrayU.empty remaining_cases) in

	ArrayU.iter decls begin function
		| Ast.Fn((_, _, _, body) as fn) ->
			let return_type = (ft_of_fn fn).N.return_type in
			assert_value_assignable return_type body
		| Ast.Cn((loc, _, type_ast, parts) as _cn) ->
			let typ = declared_type type_ast in
			let case_types =
				match typ with
				| N.TFn(N.Ct {N.ct_cases; _}) -> ct_cases
				| _ -> raise U.TODO (*TODO: an appropriate error*) in
			check_cn loc case_types parts
		| Ast.Rt _ | Ast.Un _ | Ast.Ft _ | Ast.Ct _ ->
			()
	end;

	{expr_types; local_types}
