open N

module ExprTys = AstU.ExprLookup
type expr_tys = ty ExprTys.t
module LocalTys = AstU.LocalDeclareLookup
type local_tys = ty LocalTys.t

type t = {expr_tys: expr_tys; local_tys: local_tys}
let ty_of_expr({expr_tys; _}: t): Ast.expr -> ty = ExprTys.get expr_tys
let ty_of_local({local_tys; _}: t): Ast.access -> ty = LocalTys.get local_tys

type ctx = {
	res: t;
	bindings: Bind.t;
	type_of_ast: TypeOfAst.t
}

let get_local_ty({res = {local_tys; _}; _}: ctx): Ast.access -> ty =
	LocalTys.get local_tys
let set_local_ty({res = {local_tys; _}; _}: ctx): Ast.access-> ty -> unit =
	LocalTys.set local_tys
let set_expr_ty({res = {expr_tys; _}; _}: ctx): Ast.expr -> ty -> unit =
	ExprTys.set expr_tys

let binding({bindings; _}: ctx): Ast.access -> Binding.v =
	Bind.binding bindings
let ty_binding({bindings; _}: ctx): Ast.access -> Binding.ty =
	Bind.ty_binding bindings
let rt_of_ast({type_of_ast; _}: ctx): Ast.rt -> rt =
	TypeOfAst.rt_of_ast type_of_ast
let declared_ty({bindings; type_of_ast; _}: ctx): Ast.ty -> ty =
	TypeOfAst.declared_ty bindings type_of_ast
let ft_of_fn({type_of_ast; _}: ctx): Ast.fn -> ft =
	TypeOfAst.ft_of_fn type_of_ast
let parameter_ty({type_of_ast; _}: ctx): Ast.parameter -> ty =
	TypeOfAst.parameter_ty type_of_ast


type expected =
	(* Expression is used as an argument. Explicit upcast is mandated for functions. *)
	| Exact of ty
	| Convert of ty
	(* Any type OK *)
	| Infer

(*TODO:NAME*)
let assert_foo(loc: Loc.t)(expected: expected)(actual: ty): ty =
	match expected with
	| Exact ty ->
		(*TODO:RENAME THIS METHOD*)
		TypeCheckU.assert_exact loc ty actual;
		ty
	| Convert ty ->
		TypeCheckU.assert_convert loc ty actual;
		(* The expression *inside* of a conversion has a different type than the conversion expression. *)
		actual
	| Infer ->
		actual

(*
Returns the 'type' of the type when used as a value.
For example, an rt may be used as a function.
*)
let check_ty_as_expr(ctx: ctx)(expected: expected)(ty_ast: Ast.ty): ty =
	let Ast.TypeAccess((loc, _) as access) = ty_ast in
	let t =
		begin match ty_binding ctx access with
		| Binding.TDeclared d ->
			begin match d with
			| Ast.Rt rt_ast ->
				(*TODO: Factor out to fn_of_rc helper somewhere in TyU*)
				let {rname; properties} as rt = rt_of_ast ctx rt_ast in
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
		end in
	assert_foo loc expected t

(*TODO: MOVE TO UTIL*)
let check_pattern(ctx: ctx)(ty: ty)(pattern: Ast.pattern) =
	let rec recur(ty: ty) = function
		| Ast.PSingle declare ->
			set_local_ty ctx declare ty;
		| Ast.PDestruct(_, patterns) ->
			begin match ty with
			| Rt {properties; _} ->
				if (Array.length properties != Array.length patterns) then raise U.TODO;(*TODO: appropriate error*)
				ArrayU.iter_zip properties patterns begin fun (_, property_ty) pattern ->
					recur property_ty pattern
				end
			| _ ->
				raise U.TODO (*TODO: Error: can only destructure rt*)
			end in
	recur ty pattern

(*TODO:MOVE TO UTIL*)
let property_ty(loc: Loc.t)(ty: ty)(property: Sym.t): ty =
	match ty with
	| Rt({properties; _} as rt) ->
		(*property = Sym.t * ty*)
		let prop_ty = ArrayU.find_map properties begin fun (name, ty) ->
			OpU.op_if (Sym.eq name property) @@ fun () -> ty
		end in
		OpU.or_else prop_ty begin fun () ->
			ErrU.raise loc @@ Err.NoSuchProperty(rt, property)
		end

	| Any | TPrimitive _ | Un _ | TFn _ ->
		ErrU.raise loc @@ Err.NotARc(ty)

let rec assert_exact(ctx: ctx)(expected: ty)(expr: Ast.expr): unit =
	ignore @@ check_worker ctx (Exact expected) expr

and assert_parameter(ctx: ctx)((_, expected): parameter)(expr: Ast.expr): unit =
	assert_exact ctx expected expr

(* Expr must be a *subtype* of the expected type *)
(*TODO:KILLand assert_parameter_assignable(ctx: ctx)((_, parameter_ty): parameter)(expr: Ast.expr): unit =
	ignore @@ check_worker ctx (Parameter parameter_ty) expr*)

and check_and_infer(ctx: ctx)(expr: Ast.expr): ty =
	check_worker ctx Infer expr

and check_worker(ctx: ctx)(expected: expected)(expr: Ast.expr): ty =
	(*TODO: factor out some code -- this function is just too long!*)
	let expr_ty =
		match expr with
		| Ast.At(loc, kind, ty_ast, expr) ->
			let ty = declared_ty ctx ty_ast in
			ignore @@ assert_foo loc expected ty;
			let expected =
				match kind with
				| Ast.Exact -> Exact ty
				| Ast.Convert -> Convert ty in
			ignore @@ check_worker ctx expected expr;
			ty

		| Ast.ExprType(ty_ast) ->
			check_ty_as_expr ctx expected ty_ast

		| Ast.ExprAccess((loc, _) as access) ->
			let t =
				begin match binding ctx access with
				| Binding.Builtin value ->
					ValU.ty_of value
				| Binding.VDeclared d ->
					begin match d with
					| Ast.Fn fn ->
						TFn(Ft(ft_of_fn ctx fn))
					| Ast.Cn(_, _, ty, _) ->
						declared_ty ctx ty
					end
				| Binding.Local declare ->
					get_local_ty ctx declare
				| Binding.Parameter parameter ->
					parameter_ty ctx parameter
				end in
			assert_foo loc expected t

		| Ast.Call(loc, called, args) ->
			let called_ty = check_and_infer ctx called in
			let t =
				begin match called_ty with
				| TFn f ->
					begin match f with
					| Ft {fname = _; return; parameters} ->
						let n_params = Array.length parameters in
						let n_args = Array.length args in
						ErrU.check (n_params = n_args) loc @@ Err.NumArgs(n_params, n_args);
						(*TODO: use parameter name for helpful error info*)
						ArrayU.iter_zip parameters args (assert_parameter ctx);
						return
					| Ct {cname = _; ct_cases} ->
						ErrU.check (Array.length args = 1) loc @@ Err.NumArgs(1, Array.length args);
						let arg = args.(0) in
						let arg_ty = check_and_infer ctx arg in
						let found = ArrayU.find_map ct_cases begin fun (return, input) ->
							OpU.op_if (TypeCheckU.eq input arg_ty) @@ fun () -> return
						end in
						OpU.or_else found @@ fun () -> raise U.TODO(*TODO: appropriate error*)
					end
				| _ ->
					ErrU.raise loc @@ Err.NotAFunction called_ty
				end in
			assert_foo loc expected t

		| Ast.Cs(loc, cased, parts) ->
			check_cs ctx expected loc cased parts

		| Ast.GetProperty(loc, expr, property) ->
			let ty = check_and_infer ctx expr in
			let pty = property_ty loc ty property in
			assert_foo loc expected pty

		| Ast.Let(_, pattern, value, expr) ->
			let ty = check_and_infer ctx value in
			check_pattern ctx ty pattern;
			check_worker ctx expected expr

		| Ast.Literal(loc, v) ->
			assert_foo loc expected @@ TPrimitive(ValU.ty_of_primitive v)

		| Ast.Seq(_, a, b) ->
			assert_exact ctx t_void a;
			check_worker ctx expected b

		| Ast.Partial(loc, fn, args) ->
			let fn_ty = check_and_infer ctx fn in
			let t =
				begin match fn_ty with
				| TFn f ->
					begin TFn begin Ft begin match f with
					| Ft {fname; return; parameters} ->
						assert (Array.length parameters >= Array.length args); (*TODO: proper error message*)
						let remaining_parameters = ArrayU.partial parameters args (assert_parameter ctx) in
						(*
						TODO: it should have a different name, or some indication that it's partial now...
						(TODO: don't just manipulate strings! replace `fname` with structured data tracing the type's origin.
						*)
						{fname; return; parameters = remaining_parameters}
					| Ct _ ->
						raise U.TODO
					end end end
				| _ ->
					raise U.TODO (*TODO: error message*)
				end in
			assert_foo loc expected t

		| Ast.Quote(loc, _, parts) ->
			U.returning (assert_foo loc expected t_string) begin fun _ ->
				ArrayU.iter parts begin fun (interpolated, _) ->
					(*TODO: should be a String*)
					assert_exact ctx Any interpolated
				end
			end

		| Ast.Check(loc, checked) ->
			assert_exact ctx t_bool checked;
			assert_foo loc expected t_void in

	set_expr_ty ctx expr expr_ty;
	expr_ty

and check_cs(ctx: ctx)(expected: expected)(loc: Loc.t)(cased: Ast.expr)(parts: Ast.cs_part array): ty =
	let cased_tys =
		match check_and_infer ctx cased with
		| Un {utys; _} -> utys
		| t -> ErrU.raise (AstU.expr_loc cased) (Err.CanOnlyCsUnion t) in
	let remaining_tys, part_tys =
		ArrayU.fold_map cased_tys parts begin fun remaining_tys (_, (test_loc, test_ty_ast, pattern), result) ->
			let test_ty = declared_ty ctx test_ty_ast in
			check_pattern ctx test_ty pattern;
			let remaining_tys =
				match ArrayU.try_remove remaining_tys test_ty with
				| Some tys ->
					tys
				| None ->
					ErrU.raise test_loc @@ Err.CsPartType(remaining_tys, test_ty) in
			remaining_tys, check_worker ctx expected result
		end in
	ErrU.check (ArrayU.empty remaining_tys) loc @@ Err.CasesUnhandled remaining_tys;
	match expected with
	| Exact ty ->
		ty
	| Convert _ ->
		raise U.TODO (*TODO: where do we perform conversion?*)
		(*TODO: we should demand Exact type from things within the Case.*)
	| Infer ->
		TypeCheckU.join loc part_tys

(*TODO: share code with check_cs*)
let check_cn(ctx: ctx)(_loc: Loc.t)(cases: ct_case array)(parts: Ast.cs_part array): unit =
	let remaining_cases = ArrayU.fold cases parts begin fun remaining_cases (_, (_, test_ty_ast, pattern), result) ->
		let test_ty = declared_ty ctx test_ty_ast in
		check_pattern ctx test_ty pattern;
		let (_, return), remaining_cases =
			match ArrayU.try_remove_where remaining_cases @@ fun (input, _) -> TypeCheckU.eq input test_ty with
			| Some(x) -> x
			| None -> raise U.TODO (*TODO: CasePartType-like error*) in
		assert_exact ctx return result;
		remaining_cases
	end in
	(*TODO: ErrU.check (ArrayU.empty remaining_cases) loc @@ Err.CasesUnhandled remaining_cases*)
	assert (ArrayU.empty remaining_cases)

let f(bindings: Bind.t)(type_of_ast: TypeOfAst.t)((_, decls): Ast.modul): t =
	let expr_tys: expr_tys = ExprTys.create() in
	let local_tys: local_tys = LocalTys.create() in
	U.returning {expr_tys; local_tys} begin fun res ->
		let ctx = {res; bindings; type_of_ast} in
		ArrayU.iter decls begin function
			| Ast.DeclVal v ->
				begin match v with
				| Ast.Fn((_, _, _, body) as fn) ->
					let return = (TypeOfAst.ft_of_fn type_of_ast fn).return in
					assert_exact ctx return body
				| Ast.Cn((loc, _, ty_ast, parts) as _cn) ->
					let case_tys =
						match declared_ty ctx ty_ast with
						| TFn(Ct {ct_cases; _}) -> ct_cases
						| _ -> raise U.TODO (*TODO: an appropriate error*) in
					check_cn ctx loc case_tys parts
				end
			| Ast.DeclTy _ ->
				()
		end
	end
