type code_idx = int
type stack_depth = int

(* Stores the indexes of GoTo bytecodes where the label is used. *)
type label = code_idx BatDynArray.t

module ParamIndexes = AstU.ParameterLookup
module LocalDepths = AstU.LocalDeclareLookup
type local_depths = stack_depth LocalDepths.t

let write_code(get_fn: Ast.fn -> Code.fn)(bindings: Bind.t)(types: TypeCheck.t)(params: Ast.parameter array)(body: Ast.expr): Code.t =
	(* Params are at negative depth *)
	let stack_depth = ref 0 in
	let apply_fn_to_stack_depth arity =
		(* Take off args, push return value *)
		stack_depth := !stack_depth - arity + 1 in

	let param_index: Ast.parameter -> int =
		ParamIndexes.get (ParamIndexes.build_from_keys_with_index params (fun i _ -> i)) in

	let code: Code.bytecode BatDynArray.t = BatDynArray.create() in
	let next_code_idx() = BatDynArray.length code in
	let write_bc(bc: Code.bytecode): unit =
		BatDynArray.add code bc in

	let local_depths: local_depths = LocalDepths.create() in
	let set_local_depth(declare: Ast.local_declare): unit =
		LocalDepths.set local_depths declare !stack_depth in
	let get_local_depth = LocalDepths.get local_depths in

	let new_label(): label = BatDynArray.create() in
	let use_label(l: label): unit =
		BatDynArray.add l (next_code_idx()) in
	let goto(l: label): unit =
		use_label l;
		(* We don't know where to goto yet, filled in during resolve_label *)
		write_bc (Code.Goto (-1)) in
	(*TODO: rename to Cond and inline*)
	let goto_if_false(l: label): unit =
		use_label l;
		write_bc (Code.GotoIfFalse (-1));
		decr stack_depth in
	let resolve_label(l: label): unit =
		let idx = next_code_idx() in
		DynArrayU.iter l begin fun use ->
			let placeholder = BatDynArray.get code use in
			(* Write over it with the correct index. *)
			BatDynArray.set code use begin match placeholder with
			| Code.Goto -1 ->
				Code.Goto idx
			| Code.GotoIfFalse -1 ->
				Code.GotoIfFalse idx
			| _ ->
				assert false
			end
		end in

	let rec write_builtin(b: Builtin.t)(args: Ast.expr array): unit =
		match b with
		| Builtin.Cond ->
			let condition, if_true, if_false = ArrayU.triple_of_array args in
			(* First, eagerly push cond *)
			write_expr condition;

			let else_ = new_label() in
			let bottom = new_label() in

			goto_if_false else_;

			let original_stack_depth = !stack_depth in

			write_expr if_true;
			goto bottom;

			(* This will only be reached for `false`, so ignore the value that would have been written for `true`. *)
			decr stack_depth;

			resolve_label else_;
			write_expr if_false;

			(* Stack effect is same for true and false branches, so leave effect of false branch alone. *)
			assert (!stack_depth = original_stack_depth + 1);
			resolve_label bottom

		| _ ->
			(* Standard, eager fn call *)
			ArrayU.iter args write_expr;
			write_bc (Code.CallBuiltin b);
			(* Subtract parameters, add return value *)
			apply_fn_to_stack_depth (BuiltinU.arity b)

	and write_expr(expr: Ast.expr): unit =
		match expr with
		| Ast.ExprAccess(Ast.Access(loc, _) as access) ->
			begin match Bind.binding bindings access with
			| Binding.Builtin b ->
				write_bc (Code.Const (BuiltinU.value b))
			| Binding.Declared _ ->
				raise U.TODO
			| Binding.Local l ->
				write_bc (Code.Load (get_local_depth l))
			| Binding.Parameter p ->
				write_bc (Code.Load (param_index p - Array.length params))
			| Binding.BuiltinType _ ->
				CompileErrorU.raise loc CompileError.CantUseTypeAsValue
			end;
			incr stack_depth

		| Ast.Call(_, called, args) ->
			begin match called with
			| Ast.ExprAccess(Ast.Access(loc, _) as access) ->
				(*TODO: `binding` helper close over bindings*)
				begin match Bind.binding bindings access with
				| Binding.Builtin b ->
					write_builtin b args
				| Binding.Declared d ->
					begin match d with
					| Ast.DeclFn fn_ast ->
						ArrayU.iter args write_expr;
						let fn = get_fn fn_ast in
						write_bc (Code.Call fn);
						apply_fn_to_stack_depth (CodeU.fn_arity fn)
					| Ast.DeclRc rc_ast ->
						ArrayU.iter args write_expr;
						let rc = TypeCheck.rc_of_ast types rc_ast in
						write_bc (Code.Construct rc);
						apply_fn_to_stack_depth (TypeU.rc_arity rc)
					end
				| Binding.Local _ ->
					raise U.TODO (* lambda *)
				| Binding.Parameter _ ->
					raise U.TODO (* lambda *)
				| Binding.BuiltinType _ ->
					CompileErrorU.raise loc CompileError.CantUseTypeAsValue
				end
			| _ ->
				raise U.TODO
			end

		| Ast.Case(_, cased, parts) ->
			write_expr cased;

			(* TODO: this seems neater than use_label, let's just do this all the time! *)
			let case_idx = next_code_idx() in
			let bottom = new_label() in
			write_bc (Code.Case [| |]); (* dummy *)

			let code_parts = ArrayU.map parts begin fun (Ast.CasePart(_, test, result)) ->
				let Ast.AsTest(_, local, _) = test in
				let typ = TypeCheck.type_of_local types local in
				let part_start_idx = next_code_idx() in
				(* Code.Case does not pop anything from the stack, so that becomes the new local! *)
				set_local_depth local;
				write_expr result;
				(*TODO: if this is the last part, we should already be there.*)
				goto bottom;
				(* We don't want the stack depth increased for every individual case, just once for the whole thing. *)
				decr stack_depth;
				typ, part_start_idx
			end in

			BatDynArray.set code case_idx (Code.Case code_parts);
			(* The Case replaces its input with its result, so no stack_depth effect. *)
			resolve_label bottom;
			(* Pop the cased value out from under the result *)
			write_bc Code.UnLet

		| Ast.Let(_, declare, value, expr) ->
			(* Remember the depth of the value. *)
			set_local_depth declare;
			write_expr value;
			write_expr expr;
			write_bc Code.UnLet;
			decr stack_depth

		| Ast.Literal(_, value) ->
			write_bc (Code.Const value);
			incr stack_depth

		| Ast.Seq(_, a, b) ->
			write_expr a;
			write_bc Code.Drop;
			write_expr b in

	write_expr body;

	Assert.equal !stack_depth 1 OutputU.output_int;
	write_bc Code.Return;
	BatDynArray.to_array code

module Fns = AstU.FnLookup
type fns = Code.fn Fns.t

let f(Ast.Modul(decls))(bindings: Bind.t)(types: TypeCheck.t): Modul.t =
	let fns: fns = Fns.build_from_keys (AstU.modul_fns decls) begin fun (Ast.Fn(_, name, Ast.Signature(_, _, params), _) as fn) ->
		let { Type.return_type; Type.parameters = param_types } = TypeCheck.type_of_fn types fn in
		{
			Code.fname = name;
			Code.return_type;
			Code.params = ArrayU.map_zip params param_types begin fun (Ast.Parameter(_, name, _)) typ ->
				{ Code.param_name = name; Code.param_type = typ }
			end;
			Code.code = [| |]
		}
	end in
	Fns.iter fns begin fun (Ast.Fn(_, _, Ast.Signature(_, _, params), body)) fn_code ->
		fn_code.Code.code <- write_code (Fns.get fns) bindings types params body
	end;
	{ Modul.recs = TypeCheck.all_rcs types; Modul.fns = Fns.values fns }
