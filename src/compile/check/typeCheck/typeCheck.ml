open N

module ExprTypes = AstU.ExprLookup
type expr_types = ty ExprTypes.t
module LocalTypes = AstU.LocalDeclareLookup
type local_types = ty LocalTypes.t

type t = {expr_types: expr_types; local_types: local_types}
let type_of_expr({expr_types; _}: t) = ExprTypes.get expr_types
let type_of_local({local_types; _}: t) = LocalTypes.get local_types

let f(bindings: Bind.t)(type_of_ast: TypeOfAst.t)((_, decls): Ast.modul): t =
	let declared_type = TypeOfAst.declared_type bindings type_of_ast in

	let expr_types: expr_types = ExprTypes.create() in
	let local_types: local_types = LocalTypes.create() in
	let get_local_type = LocalTypes.get local_types in
	let set_local_type = LocalTypes.set local_types in

	let rec assert_value_assignable(expected: ty)(expr: Ast.expr): unit =
		let actual = check_expr expr in
		TypeCheckU.assert_value_assignable (AstU.expr_loc expr) expected actual

	(* Expr must be a *subtype* of the expected type *)
	and assert_parameter_assignable((_, expected): parameter)(expr: Ast.expr): unit =
		let actual = check_expr expr in
		TypeCheckU.assert_parameter_assignable (AstU.expr_loc expr) expected actual

	(*TODO: move? does not recursively call others...*)
	(*
	Returns the 'type' of the type when used as a value.
	For example, an rt may be used as a function.
	*)
	and check_type_as_expr(typ_ast: Ast.typ): ty =
		let Ast.TypeAccess((loc, _) as access) = typ_ast in
		begin match Bind.ty_binding bindings access with
		| Binding.TDeclared d ->
			begin match d with
			| Ast.Rt rt_ast ->
				(*TODO: Factor out to fn_of_rc helper somewhere in TyU*)
				let {rname; properties} as rt = TypeOfAst.rt_of_ast type_of_ast rt_ast in
				TyU.t_ft rname (Rt rt) properties
			| Ast.Un _ | Ast.Ft _ | Ast.Ct _ ->
				raise U.TODO (*TODO: these types are not useable as values; have an error message saying this*)
			end
		| Binding.BuiltinType t ->
			(*TODO: type-as-value helper*)
			begin match t with
			| Rt({rname; properties} as rt) ->
				(*TODO: duplicate code of above*)
				TyU.t_ft rname (Rt rt) properties
			| _ ->
				ErrU.raise loc @@ Err.NotAValue access
			end
		end

	and check_expr(expr: Ast.expr): ty =
		(*TODO: factor out some code -- this function is just too long!*)
		let expr_type =
			match expr with
			| Ast.At(loc, typ_ast, expr) ->
				let typ = declared_type typ_ast in
				let expr_typ = check_expr expr in
				TypeCheckU.assert_upcast loc typ expr_typ;
				typ
			| Ast.ExprType(typ_ast) ->
				check_type_as_expr typ_ast
			| Ast.ExprAccess(access) ->
				begin match Bind.binding bindings access with
				| Binding.Builtin value ->
					ValU.type_of value
				| Binding.VDeclared d ->
					begin match d with
					| Ast.Fn fn ->
						TFn(Ft(TypeOfAst.ft_of_fn type_of_ast fn))
					| Ast.Cn(_, _, typ, _) ->
						declared_type typ
					end
				| Binding.Local declare ->
					get_local_type declare
				| Binding.Parameter parameter ->
					TypeOfAst.parameter_type type_of_ast parameter
				end

			| Ast.Call(loc, called, args) ->
				let called_type = check_expr called in
				begin match called_type with
				| TFn f ->
					begin match f with
					| Ft {fname = _; return_type; parameters} ->
						let n_params = Array.length parameters in
						let n_args = Array.length args in
						ErrU.check (n_params = n_args) loc @@ Err.NumArgs(n_params, n_args);
						(*TODO: use parameter name for helpful error info*)
						ArrayU.iter_zip parameters args assert_parameter_assignable;
						return_type
					| Ct {cname = _; ct_cases} ->
						ErrU.check (Array.length args = 1) loc @@ Err.NumArgs(1, Array.length args);
						let arg = args.(0) in
						let arg_type = check_expr arg in
						let found = ArrayU.find_map ct_cases begin fun (return, input) ->
							OpU.op_if (TypeCheckU.eq input arg_type) @@ fun () -> return
						end in
						OpU.or_else found @@ fun () -> raise U.TODO(*TODO: appropriate error*)
					end
				| _ ->
					ErrU.raise loc @@ Err.NotAFunction called_type
				end

			| Ast.Cs(loc, cased, parts) ->
				check_cs loc cased parts

			| Ast.Let(_, pattern, value, expr) ->
				let ty = check_expr value in
				check_pattern ty pattern;
				check_expr expr

			| Ast.Literal(_, v) ->
				TPrimitive(ValU.type_of_primitive v)

			| Ast.Seq(_, a, b) ->
				assert_value_assignable t_void a;
				check_expr b

			| Ast.Partial(_, fn, args) ->
				let fn_typ = check_expr fn in
				begin match fn_typ with
				| TFn f ->
					begin TFn begin Ft begin match f with
					| Ft {fname; return_type; parameters} ->
						assert (Array.length parameters >= Array.length args); (*TODO: proper error message*)
						let remaining_parameters = ArrayU.partial parameters args assert_parameter_assignable in
						(*
						TODO: it should have a different name, or some indication that it's partial now...
						(TODO: don't just manipulate strings! replace `fname` with structured data tracing the type's origin.
						*)
						{fname; return_type; parameters = remaining_parameters}
					| Ct _ ->
						raise U.TODO
					end end end
				| _ ->
					raise U.TODO (*TODO: error message*)
				end

			| Ast.Quote(_, _, parts) ->
				ArrayU.iter parts begin fun (interpolated, _) ->
					assert_value_assignable Any interpolated
				end;
				t_string

			| Ast.Check(_, checked) ->
				assert_value_assignable t_bool checked;
				t_void in

		ExprTypes.set expr_types expr expr_type;
		expr_type

	(*TODO:move?*)
	and check_pattern(ty: ty)(pattern: Ast.pattern) =
		match pattern with
		| Ast.PSingle declare ->
			set_local_type declare ty;
		| Ast.PDestruct(_, patterns) ->
			begin match ty with
			| Rt {properties; _} ->
				if (Array.length properties != Array.length patterns) then raise U.TODO;(*TODO: appropriate error*)
				ArrayU.iter_zip properties patterns begin fun (_, property_type) pattern ->
					check_pattern property_type pattern
				end
			| _ ->
				raise U.TODO (*TODO: Error: can only destructure rt*)
			end

	and check_cs(loc: Loc.t)(cased: Ast.expr)(parts: Ast.cs_part array): ty =
		let cased_types =
			match check_expr cased with
			| Un {utypes; _} -> utypes
			| t -> ErrU.raise (AstU.expr_loc cased) (Err.CanOnlyCsUnion t) in
		let remaining_types, part_types =
			ArrayU.fold_map cased_types parts begin fun remaining_types (_, (test_loc, test_type_ast, pattern), result) ->
				let test_type = declared_type test_type_ast in
				check_pattern test_type pattern;
				let remaining_types =
					match ArrayU.try_remove remaining_types test_type with
					| Some types ->
						types
					| None ->
						ErrU.raise test_loc @@ Err.CsPartType(remaining_types, test_type) in
				remaining_types, check_expr result
			end in
		ErrU.check (ArrayU.empty remaining_types) loc @@ Err.CasesUnhandled remaining_types;
		TypeCheckU.join loc part_types

	(*TODO: share code with check_cs*)
	and check_cn(_loc: Loc.t)(cases: ct_case array)(parts: Ast.cs_part array): unit =
		let remaining_cases = ArrayU.fold cases parts begin fun remaining_cases (_, (_, test_type_ast, pattern), result) ->
			let test_type = declared_type test_type_ast in
			check_pattern test_type pattern;
			let (_, return), remaining_cases =
				match ArrayU.try_remove_where remaining_cases @@ fun (input, _) -> TypeCheckU.eq input test_type with
				| Some(x) -> x
				| None -> raise U.TODO (*TODO: CasePartType-like error*) in
			assert_value_assignable return result;
			remaining_cases
		end in
		(*TODO: ErrU.check (ArrayU.empty remaining_cases) loc @@ Err.CasesUnhandled remaining_cases*)
		assert (ArrayU.empty remaining_cases) in

	ArrayU.iter decls begin function
		| Ast.DeclVal v ->
			begin match v with
			| Ast.Fn((_, _, _, body) as fn) ->
				let return_type = (TypeOfAst.ft_of_fn type_of_ast fn).return_type in
				assert_value_assignable return_type body
			| Ast.Cn((loc, _, type_ast, parts) as _cn) ->
				let typ = declared_type type_ast in
				let case_types =
					match typ with
					| TFn(Ct {ct_cases; _}) -> ct_cases
					| _ -> raise U.TODO (*TODO: an appropriate error*) in
					check_cn loc case_types parts
			end
		| Ast.DeclTy _ ->
			()
	end;

	{expr_types; local_types}
