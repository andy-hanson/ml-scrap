module W = CodeWriter

(*TODO: move to CodeWriter.ml*)
let declared_ty(w: W.t)(ty_ast: Ast.ty): N.ty =
	TypeOfAst.declared_ty (W.bindings w) (W.type_of_ast w) ty_ast

let rec write_expr(w: W.t)(expr: Ast.expr): unit =
	let recur = write_expr w in
	match expr with
	| Ast.At(loc, kind, ty_ast, expr) ->
		recur expr;
		begin match kind with
		| Ast.Exact -> ()
		| Ast.Convert ->
			let ty = declared_ty w ty_ast in
			let e_ty = TypeCheck.ty_of_expr (W.tys w) expr in
			begin match ty with
			| N.Rt({N.properties; _} as rt) ->
				begin match e_ty with
				| N.Rt {N.properties = e_properties; _} ->
					let indexes = ArrayU.map properties begin fun (name, _) ->
						OutputU.printf "%a\n" (ArrayU.output @@ fun out (n, _) -> Sym.output out n) e_properties;

						OpU.force @@ ArrayU.find_index e_properties @@ fun (e_name, _) ->
							OutputU.printf "%a %a\n" Sym.output name Sym.output e_name;
							Sym.eq name e_name
					end in
					OutputU.printf "%a\n" (ArrayU.output OutputU.output_int) indexes ;
					W.cnv_rc w loc rt indexes
				| _ ->
					assert false
				end
			| N.Ft _ ->
				raise U.TODO
			| _ ->
				assert false
			end
		end
	| Ast.ExprType(ty_ast) ->
		let loc, access = match ty_ast with
			| Ast.TyAccess((loc, _) as access) -> loc, access
			| _ -> raise U.TODO in
		let rt =
			begin match Bind.ty_binding (W.bindings w) access with
			| Binding.TDeclared d ->
				begin match d with
				| Ast.Rt rt_ast ->
					TypeOfAst.rt_of_ast (W.type_of_ast w) rt_ast
				| _ ->
					assert false
				end
			| Binding.ExternalTy t ->
				begin match t with
				| N.Rt rt -> rt
				| _ -> assert false
				end
			end in
		W.const w loc @@ N.Fn(N.Ctr rt)

	| Ast.ExprAccess((loc, _) as access) ->
		begin match Bind.binding (W.bindings w) access with
		| Binding.External v ->
			W.const w loc v
		| Binding.VDeclared decl ->
			let v = TypeOfAst.val_of_ast (W.type_of_ast w) decl in
			W.const w loc v
		| Binding.Local l ->
			W.access_local w loc l
		| Binding.Parameter p ->
			W.access_parameter w loc p
		end

	| Ast.Call(loc, called, args) ->
		let eager() =
			ArrayU.iter args (write_expr w);
			write_expr w called;
			W.call w loc @@ Array.length args in
		begin match called with
		| Ast.ExprAccess(access) ->
			begin match Bind.binding (W.bindings w) access with
			| Binding.External b ->
				begin match b with
				| N.Fn N.BuiltinFn(fn) when fn == Builtin.cond_value ->
					write_cond w loc args
				| _ ->
					eager()
				end
			| _ ->
				eager()
			end
		| _ ->
			eager()
		end

	| Ast.Cs(loc, cased, parts) ->
		recur cased;
		write_cs_body w loc parts

	| Ast.GetProperty(loc, expr, property) ->
		recur expr;
		begin match TypeCheck.ty_of_expr (W.tys w) expr with
		| N.Rt {N.properties; _} ->
			let index = OpU.force @@ ArrayU.find_index properties @@ fun (name, _) -> Sym.eq name property in
			W.get_property w loc index
		| _ ->
			assert false
		end

	| Ast.Let(loc, pattern, value, expr) ->
		recur value;
		let pattern_size = write_pattern w pattern in
		recur expr;
		W.un_let w loc pattern_size

	| Ast.Literal(loc, value) ->
		W.const w loc @@ N.Primitive begin match value with
			| Ast.Int i -> N.Int i
			| Ast.Float f -> N.Float f
			| Ast.String s -> N.String s
			end

	| Ast.Seq(loc, a, b) ->
		recur a;
		W.drop w loc;
		recur b

	| Ast.Partial(loc, f, args) ->
		ArrayU.iter args recur;
		recur f;
		W.partial w loc @@ Array.length args

	| Ast.Quote(loc, start, parts) ->
		ArrayU.iter parts (fun (expr, _) -> recur expr);
		let strings = Array.append [| start |] @@ ArrayU.map parts snd in
		W.quote w loc strings

	| Ast.Check(loc, expr) ->
		recur expr;
		W.check w loc

	| Ast.GenInst(_loc, _expr, _tys) ->
		(*TODO: this will need a runtime component since we need to alter the type of the function.*)
		raise U.TODO

and write_cond(w: W.t)(loc: Loc.t)(args: Ast.expr array): unit =
	let condition, if_true, if_false = ArrayU.triple_of args in
	write_expr w condition;
	let goto_else = W.placeholder w loc in
	(* condition popped off by goto_else *)
	W.decr_stack_depth w;

	write_expr w if_true;
	let goto_bottom = W.placeholder w loc in

	(* 'true' branch increments stack by 1, 'false' branch does so separately. *)
	W.decr_stack_depth w;

	W.resolve_goto_if_false w goto_else;
	write_expr w if_false;

	W.resolve_goto w goto_bottom

(* Returns number of local variables (= size of UnLet needed after using the variables) *)
and write_pattern(w: W.t)(pattern: Ast.pattern): int =
	match pattern with
	| Ast.PSingle declare ->
		(*TODO: this is silly...*)
		(*The local is the previous stack depth*)
		W.decr_stack_depth w;
		W.set_local_depth w declare;
		W.incr_stack_depth w;
		1
	| Ast.PDestruct(loc, patterns) ->
		let pattern_size = ref 0 in
		let rec pattern_of_ast(pattern: Ast.pattern): N.pattern =
			match pattern with
			| Ast.PSingle declare ->
				W.set_local_depth w declare;
				incr pattern_size;
				W.incr_stack_depth w;
				N.PSingle
			| Ast.PDestruct(_, patterns) ->
				N.PDestruct(ArrayU.map patterns pattern_of_ast) in
		(* The original value destructed is popped and destructed to the patterned values. *)
		W.decr_stack_depth w;
		let patterns = ArrayU.map patterns pattern_of_ast in
		W.destruct w loc patterns;
		!pattern_size

and write_cs_body(w: W.t)(loc: Loc.t)(parts: Ast.cs_part array): unit =
	let cases = W.cs w loc @@ Array.length parts in

	let bottom_placeholders =
		ArrayU.mapi parts begin fun idx (loc, (_, ty, pattern), result) ->
			W.resolve_cs_part w cases idx (declared_ty w ty);
			let pattern_size = write_pattern w pattern in
			write_expr w result;
			W.un_let w loc pattern_size;
			(* Like for cond, we don't want the stack depth increased for every individual case, just for the whole. *)
			(*if idx != Array.length parts - 1 then W.decr_stack_depth w;*)
			(*TODO: very last one doesn't need goto bottom, it's already at the bottom*)
			W.placeholder w loc
		end in

	ArrayU.iter bottom_placeholders @@ W.resolve_goto w

let f(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(tys: TypeCheck.t)((_, decls): Ast.modul): unit =
	ArrayU.iter decls begin function
		| Ast.DeclVal v ->
			begin match v with
			| Ast.Fn((_, _, (_, _, parameters), body) as fn_ast) ->
				let fn = TypeOfAst.fn_of_ast type_of_ast fn_ast in
				fn.N.fn_code <- W.write bindings type_of_ast tys parameters (AstU.expr_loc body) @@ fun w -> write_expr w body
			end
		| Ast.DeclTy _ ->
			()
	end;
