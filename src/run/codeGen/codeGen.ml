module W = CodeWriter

let rec write_expr(w: W.t)(expr: Ast.expr): unit =
	match expr with
	| Ast.ExprAccess((loc, _) as access) ->
		begin match W.binding w access with
		| Binding.Builtin {Builtin.value; _} ->
			W.const w loc value
		| Binding.Declared _ ->
			raise U.TODO
		| Binding.Local l ->
			W.write_local_access w loc l
		| Binding.Parameter p ->
			W.write_parameter_access w loc p
		| Binding.BuiltinType _ ->
			(*TODO: handle Rc case. codeGen should never raise compile errors.*)
			CompileErrorU.raise loc CompileError.CantUseTypeAsValue
		end

	| Ast.Call(loc, called, args) ->
		write_call w loc called args

	| Ast.Case(loc, cased, parts) ->
		write_case w loc cased parts

	| Ast.Let(loc, declare, value, expr) ->
		W.set_local_depth w declare;
		write_expr w value;
		write_expr w expr;
		W.un_let w loc

	| Ast.Literal(loc, value) ->
		W.const w loc value

	| Ast.Seq(loc, a, b) ->
		write_expr w a;
		W.drop w loc;
		write_expr w b

and write_call(w: W.t)(call_loc: Loc.t)(called: Ast.expr)(args: Ast.expr array): unit =
	let arity = Array.length args in
	match called with
	| Ast.ExprAccess((loc, _) as access) ->
		(* Eager evaluation always writes args first. Cond is special. *)
		let write_args() = ArrayU.iter args (write_expr w) in
		let call_lambda(write_access: unit -> unit): unit =
			write_args();
			write_access();
			W.call_lambda w call_loc arity in
		let construct(rt: N.rt): unit =
			write_args();
			W.construct w loc rt arity in
		begin match W.binding w access with
		| Binding.Builtin b ->
			write_builtin_call w loc b args
		| Binding.Declared d ->
			begin match d with
			| Ast.DeclFn fn_ast ->
				write_args();
				let fn = W.fn_of_ast w fn_ast in
				W.call_static w loc fn arity
			| Ast.DeclCn _cn_ast ->
				raise U.TODO
			| Ast.DeclRt rt_ast ->
				construct @@ W.rt_of_ast w rt_ast
			| Ast.DeclUn _ | Ast.DeclFt _ | Ast.DeclCt _ ->
				assert false
			end
		| Binding.Local l ->
			call_lambda @@ fun () -> W.write_local_access w loc l
		| Binding.Parameter p ->
			call_lambda @@ fun () -> W.write_parameter_access w loc p
		| Binding.BuiltinType t ->
			begin match t with
			| N.Rt rt -> construct rt
			(*TODO: codeGen should never raise compile error*)
			| _ -> CompileErrorU.raise loc CompileError.CantUseTypeAsValue
			end
		end
	| _ ->
		raise U.TODO

(*TODO:inline*)
and write_builtin_call(w: W.t)(loc: Loc.t)(b: Builtin.t)(args: Ast.expr array): unit =
	if b == Builtin.cond then
		write_cond w loc args
	else begin
		match b.Builtin.value with
		| N.BuiltinFn(fn) ->
			(* Standard, eager fn call *)
			ArrayU.iter args @@ write_expr w;
			W.call_builtin w loc fn @@ Array.length args
		| _ ->
			(* Type checker should ensure only functions get here *)
			assert false
	end

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

(*TODO:INLINE*)
and write_case(w: W.t)(loc: Loc.t)(cased: Ast.expr)(parts: Ast.case_part array): unit =
	write_expr w cased;
	write_case_body w loc parts;
	(* un-let the cased value *)
	W.un_let w loc

and write_case_body(w: W.t)(loc: Loc.t)(parts: Ast.case_part array): unit =
	let cases = W.case w loc @@ Array.length parts in

	let bottom_placeholders = ArrayU.mapi parts begin fun idx (loc, test, result) ->
		let Ast.AsTest(_, local, _) = test in
		W.resolve_case_part w cases idx (W.type_of_local w local);
		(* case does not pop anything from the stack, so that becomse the new local *)
		W.set_local_depth w local;
		write_expr w result;
		(* Like for cond, we don't want the stack depth increased for every individual case, just for the whole. *)
		if idx != Array.length parts - 1 then W.decr_stack_depth w;
		(*TODO: very last one doesn't need goto bottom, it's already at the bottom*)
		W.placeholder w loc
	end in

	ArrayU.iter bottom_placeholders (W.resolve_goto w)

(*TODO: move inside f. Or better, move inside CodeWriter.ml!*)
let write_code(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(types: TypeCheck.t)(parameters: Ast.parameter array)(loc: Loc.t)(do_write: W.t -> unit): N.code =
	let w: W.t = W.create bindings type_of_ast types parameters in
	do_write w;
	W.finish w loc

let f(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(types: TypeCheck.t)(Ast.Modul(decls)): unit =
	ArrayU.iter decls begin function
		| Ast.DeclFn((_, _, (_, _, parameters), body) as fn_ast) ->
			let fn = TypeOfAst.fn_of_ast type_of_ast fn_ast in
			fn.N.code <- write_code bindings type_of_ast types parameters (AstU.expr_loc body) @@ fun w -> write_expr w body
		| Ast.DeclCn((loc, _, _, parts) as cn_ast) ->
			let cn = TypeOfAst.cn_of_ast type_of_ast cn_ast in
			cn.N.code <- write_code bindings type_of_ast types [||] loc @@ fun w -> write_case_body w loc parts
		| Ast.DeclRt _ | Ast.DeclUn _ | Ast.DeclFt _ | Ast.DeclCt _ ->
			()
	end;
