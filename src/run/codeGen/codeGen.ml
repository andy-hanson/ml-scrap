module W = CodeWriter

let rec write_expr(w: W.t)(expr: Ast.expr): unit =
	let recur = write_expr w in
	match expr with
	| Ast.At(_, _, expr) ->
		recur expr
	| Ast.ExprType(typ_ast) ->
		let Ast.TypeAccess((loc, _) as access) = typ_ast in
		let rt =
			begin match Bind.ty_binding (W.bindings w) access with
			| Binding.TDeclared d ->
				begin match d with
				| Ast.Rt rt_ast ->
					TypeOfAst.rt_of_ast (W.type_of_ast w) rt_ast
				| _ ->
					assert false
				end
			| Binding.BuiltinType t ->
				begin match t with
				| N.Rt rt -> rt
				| _ -> assert false
				end
			end in
		W.const w loc @@ N.Fn(N.Ctr rt)

	| Ast.ExprAccess((loc, _) as access) ->
		begin match Bind.binding (W.bindings w) access with
		| Binding.Builtin v ->
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
		write_call w loc called args

	| Ast.Cs(loc, cased, parts) ->
		recur cased;
		write_cs_body w loc parts

	| Ast.Let(loc, pattern, value, expr) ->
		recur value;
		let pattern_size = handle_pattern w pattern in
		recur expr;
		W.un_let w loc pattern_size

	| Ast.Literal(loc, value) ->
		W.const w loc @@ N.Primitive value

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

(*TODO: get rid of specialized Call bytecodes, and just always use the call_lambda case*)
and write_call(w: W.t)(loc: Loc.t)(called: Ast.expr)(args: Ast.expr array): unit =
	let eager() =
		ArrayU.iter args (write_expr w);
		write_expr w called;
		W.call w loc @@ Array.length args in
	match called with
	| Ast.ExprAccess(access) ->
		begin match Bind.binding (W.bindings w) access with
		| Binding.Builtin b ->
			begin match b with
			| N.Fn N.BuiltinFn(fn) ->
				if fn == Builtin.cond_value then
					write_cond w loc args
				(*TODO:KILL else if fn == Builtin.do_value then begin
					assert (Array.length args = 1);
					write_expr w args.(0);
					W.call w loc 0*)
				else
					eager()
			| _ ->
				assert false
			end
		| _ ->
			eager()
		end
	| _ ->
		eager()

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

(*TODO:move? and rename!*)
(*Returns number of UnLets we will need at the end*)
and handle_pattern(w: W.t)(pattern: Ast.pattern): int =
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
		ArrayU.mapi parts begin fun idx (loc, (_, typ, pattern), result) ->
			W.resolve_cs_part w cases idx (TypeOfAst.declared_type (W.bindings w) (W.type_of_ast w) typ);
			let pattern_size = handle_pattern w pattern in
			write_expr w result;
			W.un_let w loc pattern_size;
			(* Like for cond, we don't want the stack depth increased for every individual case, just for the whole. *)
			(*if idx != Array.length parts - 1 then W.decr_stack_depth w;*)
			(*TODO: very last one doesn't need goto bottom, it's already at the bottom*)
			W.placeholder w loc
		end in

	ArrayU.iter bottom_placeholders @@ W.resolve_goto w

(*TODO: move inside f. Or better, move inside CodeWriter.ml!*)
let write_code(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(types: TypeCheck.t)(parameters: Ast.parameter array)(loc: Loc.t)(do_write: W.t -> unit): N.code =
	let w: W.t = W.create bindings type_of_ast types parameters in
	do_write w;
	W.finish w loc

let f(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(types: TypeCheck.t)((_, decls): Ast.modul): unit =
	ArrayU.iter decls begin function
		| Ast.DeclVal v ->
			begin match v with
			| Ast.Fn((_, _, (_, _, parameters), body) as fn_ast) ->
				let fn = TypeOfAst.fn_of_ast type_of_ast fn_ast in
				fn.N.code <- write_code bindings type_of_ast types parameters (AstU.expr_loc body) @@ fun w -> write_expr w body
			| Ast.Cn((loc, _, _, parts) as cn_ast) ->
				let fn = TypeOfAst.cn_of_ast type_of_ast cn_ast in
				fn.N.code <- write_code bindings type_of_ast types [||] loc begin fun w ->
					(* Keep the original cased value on the stack. *)
					W.dup w loc;
					write_cs_body w loc parts
				end
			end
		| Ast.DeclTy _ ->
			()
	end;
