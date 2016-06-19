type code_idx = int
type stack_depth = int

(* Stores the indexes of GoTo bytecodes where the label is used. *)
type label = code_idx MutArray.t

let write_code
	(fn_of_ast: Ast.fn -> Val.fn)
	(bindings: Bind.t)
	(type_of_ast: TypeOfAst.t)
	(types: TypeCheck.t)
	(params: Ast.parameter array)
	(body: Ast.expr): Val.code =

	(* Params are at negative depth. *)
	(* We simulate the stack depth so we now how where locals will be located. *)
	let stack_depth = ref 0 in
	let apply_fn_to_stack_depth arity =
		(* Take off args, push return value *)
		stack_depth := !stack_depth - arity + 1 in

	let param_index: Ast.parameter -> int =
		AstU.ParameterLookup.get (AstU.ParameterLookup.build_from_keys_with_index params (fun i _ -> i)) in

	let code: Val.bytecode MutArray.t = MutArray.create() in
	let next_code_idx() = MutArray.length code in
	let locs = CodeLocs.create_builder() in
	let write_bc(loc: Loc.t)(bc: Val.bytecode): unit =
		MutArray.add code bc;
		CodeLocs.write locs loc in

	let local_depths: stack_depth AstU.LocalDeclareLookup.t = AstU.LocalDeclareLookup.create() in
	let set_local_depth(declare: Ast.local_declare): unit =
		AstU.LocalDeclareLookup.set local_depths declare !stack_depth in
	let get_local_depth = AstU.LocalDeclareLookup.get local_depths in

	let new_label(): label = MutArray.create() in
	let use_label(l: label): unit =
		MutArray.add l (next_code_idx()) in
	let goto(loc: Loc.t)(l: label): unit =
		use_label l;
		(* We don't know where to goto yet, filled in during resolve_label *)
		write_bc loc (Val.Goto (-1)) in
	let resolve_label(l: label): unit =
		let idx = next_code_idx() in
		MutArray.iter l begin fun use ->
			let placeholder = MutArray.get code use in
			(* Write over it with the correct index. *)
			MutArray.set code use begin match placeholder with
			| Val.Goto -1 ->
				Val.Goto idx
			| Val.GotoIfFalse -1 ->
				Val.GotoIfFalse idx
			| _ ->
				assert false
			end
		end in

	let rec write_builtin(loc: Loc.t)(b: Builtin.t)(args: Ast.expr array): unit =
		if b == Builtin.cond then begin
			let condition, if_true, if_false = ArrayU.triple_of_array args in
			(* First, eagerly push cond *)
			write_expr condition;

			let else_ = new_label() in
			let bottom = new_label() in

			use_label else_;
			write_bc loc (Val.GotoIfFalse (-1));
			(* condition popped off by GotoIfFalse *)
			decr stack_depth;

			let original_stack_depth = !stack_depth in

			write_expr if_true;
			goto loc bottom;

			(* This will only be reached for `false`, so ignore the value that would have been written for `true`. *)
			decr stack_depth;

			resolve_label else_;
			write_expr if_false;

			(* Stack effect is same for true and false branches, so leave effect of false branch alone. *)
			assert (!stack_depth = original_stack_depth + 1);
			resolve_label bottom
		end else begin
			match b.Builtin.value with
			| Val.BuiltinFn({Val.typ; _} as fn) ->
				(* Standard, eager fn call *)
				ArrayU.iter args write_expr;
				write_bc loc (Val.CallBuiltin fn);
				apply_fn_to_stack_depth (TypeU.ft_arity typ)
			| _ ->
				(* Type checker should ensure only functions get here *)
				assert false
		end

	and write_local_access(loc: Loc.t)(local: Ast.local_declare): unit =
		write_bc loc (Val.Load (get_local_depth local))
	and write_parameter_access(loc: Loc.t)(parameter: Ast.parameter): unit =
		write_bc loc (Val.Load (param_index parameter - Array.length params))

	and write_expr(expr: Ast.expr): unit =
		match expr with
		| Ast.ExprAccess(Ast.Access(loc, _) as access) ->
			begin match Bind.binding bindings access with
			| Binding.Builtin {Builtin.value; _} ->
				write_bc loc (Val.Const value)
			| Binding.Declared _ ->
				raise U.TODO
			| Binding.Local l ->
				write_local_access loc l
			| Binding.Parameter p ->
				write_parameter_access loc p
			| Binding.BuiltinType _ ->
				CompileErrorU.raise loc CompileError.CantUseTypeAsValue
			end;
			incr stack_depth

		| Ast.Call(call_loc, called, args) ->
			begin match called with
			| Ast.ExprAccess(Ast.Access(loc, _) as access) ->
				(* Eager evaluation always writes args first. Cond is special. *)
				let write_args() = ArrayU.iter args write_expr in
				let fn_arity(typ: Type.t): int =
					match typ with
					| Type.Ft f -> TypeU.ft_arity f
					| _ -> assert false in

				(*TODO: `binding` helper close over bindings*)
				begin match Bind.binding bindings access with
				| Binding.Builtin b ->
					write_builtin loc b args
				| Binding.Declared d ->
					begin match d with
					| Ast.DeclFn fn_ast ->
						write_args();
						let fn = fn_of_ast fn_ast in
						write_bc loc (Val.CallStatic fn);
						apply_fn_to_stack_depth (ValU.fn_arity fn)
					| Ast.DeclRc rc_ast ->
						write_args();
						let rc = TypeOfAst.rc_of_ast type_of_ast rc_ast in
						write_bc loc (Val.Construct rc);
						apply_fn_to_stack_depth (TypeU.rc_arity rc)
					| Ast.DeclUn _ | Ast.DeclFt _ ->
						assert false
					end
				| Binding.Local l ->
					write_args();
					write_local_access loc l;
					write_bc call_loc Val.CallLambda;
					apply_fn_to_stack_depth (fn_arity (TypeCheck.type_of_local types l))
				| Binding.Parameter p ->
					(*TODO:duplicate code from Binding.Local version...*)
					write_args();
					write_parameter_access loc p;
					write_bc call_loc Val.CallLambda;
					apply_fn_to_stack_depth (fn_arity (TypeCheck.type_of_parameter types p))
				| Binding.BuiltinType _ ->
					CompileErrorU.raise loc CompileError.CantUseTypeAsValue
				end
			| _ ->
				raise U.TODO
			end

		| Ast.Case(loc, cased, parts) ->
			write_expr cased;

			(* TODO: this seems neater than use_label, let's just do this all the time! *)
			let case_idx = next_code_idx() in
			let bottom = new_label() in
			write_bc loc (Val.Case [| |]); (* dummy *)

			let code_parts = ArrayU.map parts begin fun (Ast.CasePart(loc, test, result)) ->
				let Ast.AsTest(_, local, _) = test in
				let typ = TypeCheck.type_of_local types local in
				let part_start_idx = next_code_idx() in
				(* Val.Case does not pop anything from the stack, so that becomes the new local! *)
				set_local_depth local;
				write_expr result;
				(*TODO: if this is the last part, we should already be there.*)
				goto loc bottom;
				(* We don't want the stack depth increased for every individual case, just once for the whole thing. *)
				decr stack_depth;
				typ, part_start_idx
			end in

			MutArray.set code case_idx (Val.Case code_parts);
			(* The Case replaces its input with its result, so no stack_depth effect. *)
			resolve_label bottom;
			(* Pop the cased value out from under the result *)
			write_bc loc Val.UnLet

		| Ast.Let(loc, declare, value, expr) ->
			(* Remember the depth of the value. *)
			set_local_depth declare;
			write_expr value;
			write_expr expr;
			write_bc loc Val.UnLet;
			decr stack_depth

		| Ast.Literal(loc, value) ->
			write_bc loc (Val.Const value);
			incr stack_depth

		| Ast.Seq(loc, a, b) ->
			write_expr a;
			write_bc loc Val.Drop; decr stack_depth;
			write_expr b in

	write_expr body;

	Assert.equal !stack_depth 1 OutputU.output_int;
	write_bc (AstU.expr_loc body) Val.Return;
	{Val.bytecodes = MutArray.to_array code; Val.locs = CodeLocs.finish locs}

module Fns = AstU.FnLookup
type fns = Val.fn Fns.t

let f(file_name: FileIO.file_name)(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(types: TypeCheck.t)(Ast.Modul(decls)): Val.modul =
	let rcs, uns, fts = TypeOfAst.all type_of_ast in
	U.returning {Val.file_name; Val.fns = [||]; Val.rcs; Val.uns; Val.fts} begin fun modul ->
		let fns: fns = Fns.build_from_keys (AstU.modul_fns decls) begin fun fn ->
			{
				Val.info = {
					ft = TypeCheck.type_of_fn types fn;
					Val.containing_modul = modul;
					Val.code = {Val.bytecodes = [||]; Val.locs = CodeLocs.empty}
				}
			}
		end in
		Fns.iter fns begin fun (Ast.Fn(_, _, Ast.Signature(_, _, params), body)) fn_code ->
			fn_code.Val.info.Val.code <- write_code (Fns.get fns) bindings type_of_ast types params body
		end;
		modul.Val.fns <- Fns.values fns
	end
