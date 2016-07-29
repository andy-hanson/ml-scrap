open N.V
open N.Ty
open N.TyP

module ExprTys = AstLookup.Expr
type expr_tys = ty ExprTys.t
module LocalTys = AstLookup.LocalDeclare
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
let declared_ty({bindings; type_of_ast; _}: ctx): Ast.ty -> ty =
	TypeOfAst.declared_ty bindings type_of_ast
let parameter_ty({type_of_ast; _}: ctx): Ast.parameter -> ty =
	TypeOfAst.parameter_ty type_of_ast




type expected =
	(* Value must be exactly of the given type or, if ty is a union, a member of that union. *)
	| Exact of ty
	(* Expression is in `A @ e`, so it must be a value convertible to A. *)
	| Convert of ty
	(* Nothing particular is expected. *)
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

(*TODO: move to TyU?*)
let ft_of_rt_ctr({rt_origin = _; properties} as rt: rt): ft =
	{
		ft_origin = FtFromRt rt;
		return = Rt rt;
		parameters = properties
	}

(*
Returns the 'type' of the type when used as a value.
For example, an rt may be used as a function.
*)
let check_ty_as_expr(ctx: ctx)(expected: expected)(ty_ast: Ast.ty): ty =
	let ty = declared_ty ctx ty_ast in
	let ty_as_expr =
		match ty with
		| Rt rt ->
			Ft(ft_of_rt_ctr rt)
		| _ ->
			U.todo() (*TODO: error message*) in
	assert_foo (AstU.ty_loc ty_ast) expected ty_as_expr

(*TODO: MOVE TO UTIL*)
let check_pattern(ctx: ctx)(ty: ty)(pattern: Ast.pattern): unit =
	U.loop2 ty pattern @@ fun loop ty -> function
		| Ast.PSingle declare ->
			set_local_ty ctx declare ty
		| Ast.PDestruct(_, patterns) ->
			begin match ty with
			| Rt {properties; _} ->
				if not @@ ArrayU.same_length properties patterns then U.todo();(*TODO: appropriate error*)
				ArrayU.iter_zip properties patterns @@ fun (_, property_ty) pattern ->
					loop property_ty pattern
			| _ ->
				U.todo() (*TODO: Error: can only destructure rt*)
			end

(*TODO:MOVE TO UTIL*)
let property_ty(loc: Loc.t)(ty: ty)(property: Sym.t): ty =
	match ty with
	| Rt({properties; _} as rt) ->
		(*property = Sym.t * ty*)
		let prop_ty = ArrayU.find_map properties @@ fun (name, ty) ->
			OpU.op_if (Sym.eq name property) @@ fun () -> ty in
		OpU.or_else prop_ty @@ fun () ->
			ErrU.raise loc @@ Err.NoSuchProperty(rt, property)
	| _ ->
		ErrU.raise loc @@ Err.NotARc(ty)

let rec assert_exact(ctx: ctx)(expected: ty)(expr: Ast.expr): unit =
	ignore @@ check_worker ctx (Exact expected) expr

and assert_parameter(ctx: ctx)((_, expected): parameter)(expr: Ast.expr): unit =
	assert_exact ctx expected expr

and check_and_infer(ctx: ctx)(expr: Ast.expr): ty =
	check_worker ctx Infer expr

and check_worker(ctx: ctx)(expected: expected)(expr: Ast.expr): ty =
	let expr_ty =
		match expr with
		| Ast.At(loc, kind, ty_ast, expr) ->
			let ty = declared_ty ctx ty_ast in
			ignore @@ assert_foo loc expected ty;
			let expected =
				match kind with
				| Ast.Exact -> Exact ty
				| Ast.Convert -> Convert ty in
			(*TODO: get converted type and assert that we actually performed a conversion somewhere.*)
			ignore @@ check_worker ctx expected expr;
			ty

		| Ast.ExprType(ty_ast) ->
			check_ty_as_expr ctx expected ty_ast

		| Ast.ExprAccess((loc, _) as access) ->
			let t =
				begin match binding ctx access with
				| Binding.External value ->
					ValU.ty_of value
				| Binding.VDeclared d ->
					begin match d with
					| Ast.Fn fn_ast ->
						let fn = TypeOfAst.fn_of_ast ctx.type_of_ast fn_ast in
						TyU.ty_of_ft_or_gen fn.fn_ty
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
				| Ft {ft_origin = _; return; parameters} ->
					let n_params = Array.length parameters in
					let n_args = Array.length args in
					ErrU.check (n_params = n_args) loc @@ Err.NumArgs(n_params, n_args);
					(*TODO: use parameter name for helpful error info*)
					ArrayU.iter_zip parameters args (assert_parameter ctx);
					return
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
			let prim = begin match v with
				| Ast.Int _ -> t_int
				| Ast.Float _ -> t_float
				| Ast.String _ -> t_string
				end in
			assert_foo loc expected prim

		| Ast.Seq(_, a, b) ->
			assert_exact ctx t_void a;
			check_worker ctx expected b

		| Ast.Partial(loc, fn, args) ->
			let fn_ty = check_and_infer ctx fn in
			let t =
				begin match fn_ty with
				| Ft({parameters; _} as ft) ->
					assert (Array.length parameters >= Array.length args); (*TODO: proper error message*)
					ArrayU.partial_iter parameters args (assert_parameter ctx);
					Ft(TyU.partial ft @@ Array.length args)
				| _ ->
					U.todo() (*TODO: error message*)
				end in
			assert_foo loc expected t

		| Ast.Quote(loc, _, parts) ->
			U.returning (assert_foo loc expected t_string) @@ fun _ ->
				ArrayU.iter parts @@ fun (interpolated, _) ->
					assert_exact ctx t_string interpolated

		| Ast.Check(loc, checked) ->
			assert_exact ctx t_bool checked;
			assert_foo loc expected t_void

		| Ast.GenInst(loc, expr, ty_asts) ->
			let ty = check_and_infer  ctx expr in
			(*TODO: this means TypeOfAst does typechecking work... could I be neater about this?*)
			TypeOfAst.instantiate_generic ctx.bindings ctx.type_of_ast loc ty ty_asts in

	set_expr_ty ctx expr expr_ty;
	expr_ty

and check_cs(ctx: ctx)(expected: expected)(loc: Loc.t)(cased: Ast.expr)(parts: Ast.cs_part array): ty =
	let cased_tys =
		match check_and_infer ctx cased with
		| Un {utys; _} -> utys
		| t -> ErrU.raise (AstU.expr_loc cased) @@ Err.CanOnlyCsUnion t in
	let remaining_tys, part_tys =
		ArrayU.fold_map cased_tys parts @@ fun remaining_tys (_, (test_loc, test_ty_ast, pattern), result) ->
			let test_ty = declared_ty ctx test_ty_ast in
			check_pattern ctx test_ty pattern;
			let remaining_tys =
				match ArrayU.try_remove remaining_tys test_ty with
				| Some tys ->
					tys
				| None ->
					ErrU.raise test_loc @@ Err.CsPartType(remaining_tys, test_ty) in
			remaining_tys, check_worker ctx expected result in
	ErrU.check (ArrayU.empty remaining_tys) loc @@ Err.CasesUnhandled remaining_tys;
	match expected with
	| Exact ty ->
		ty
	| Convert _ ->
		U.todo() (*TODO: where do we perform conversion?*)
		(*TODO: we should demand Exact type from things within the Case.*)
	| Infer ->
		TypeCheckU.join loc part_tys

let f(bindings: Bind.t)(type_of_ast: TypeOfAst.t)((_, decls): Ast.modul): t =
	U.returning {expr_tys = ExprTys.create(); local_tys = LocalTys.create()} @@ fun res ->
		let ctx = {res; bindings; type_of_ast} in
		ArrayU.iter decls @@ function
			| Ast.DeclVal v ->
				begin match v with
				| Ast.Fn((_, _, _, body) as fn_ast) ->
					let fn = TypeOfAst.fn_of_ast type_of_ast fn_ast in
					match fn.fn_ty with
					| FoG_Ft ft ->
						assert_exact ctx ft.return body
					| FoG_Gen g ->
						assert_exact ctx g.gen_ft_return body
				end
			| Ast.DeclTy _ ->
				()
