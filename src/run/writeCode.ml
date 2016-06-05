(* Stores the index of GoTo bytecodes where the label is used. *)
type label = int BatDynArray.t

(* Write a function body. *)
let write_code(get_func: Ast.decl_val -> Code.func)(bindings: Bind.t)(types: TypeCheck.t)(params: Ast.local_declare array)(e: Ast.expr): Code.t =
	let stack_depth = ref 0 in
	let apply_fn_to_stack_depth arity =
		(* Take off args, push return value *)
		U.mod_ref stack_depth (fun s -> s - arity + 1) in

	let code: Code.bytecode BatDynArray.t = BatDynArray.create() in
	let next_code_idx() = BatDynArray.length code in
	let write_bc(bc: Code.bytecode): unit =
		(* Batteries.Printf.printf "WRITE %a, depth: %d\n" Code.output_code bc !stack_depth; *)
		BatDynArray.add code bc in

	let local_depths: (Ast.local_declare, int) Hashtbl.t = Hashtbl.create 0 in
	let set_local_depth(declare: Ast.local_declare): unit =
		Hashtbl.add local_depths declare !stack_depth in
	let get_local_depth(declare: Ast.local_declare): int =
		Hashtbl.find local_depths declare in


	let new_label(): label = BatDynArray.create() in
	let use_label(l: label): unit =
		BatDynArray.add l (next_code_idx()) in
	let goto(l: label): unit =
		use_label l;
		(* We don't know where to goto yet, filled in during resolve_label *)
		write_bc (Code.Goto (-1)) in
	let goto_if_false(l: label): unit =
		use_label l;
		write_bc (Code.GotoIfFalse (-1));
		decr stack_depth in
	let resolve_label(l: label): unit =
		let idx = next_code_idx() in
		let write_jump_target use =
			let placeholder = BatDynArray.get code use in
			(* Write over it with the correct index. *)
			BatDynArray.set code use begin match placeholder with
			| Code.Goto -1 ->
				Code.Goto idx
			| Code.GotoIfFalse -1 ->
				Code.GotoIfFalse idx
			| _ -> failwith "Unrecognized jump code"
			end in
		BatDynArray.iter write_jump_target l in

	let rec write_builtin(b: Builtins.builtin)(args: Ast.expr array): unit =
		match b with
		| Builtins.Cond ->
			let condition, if_true, if_false = U.arr3 args in
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
			(* Standard, eager function call *)
			Array.iter write_expr args;
			write_bc (Code.CallBuiltin b);
			(* Subtract parameters, add return value *)
			apply_fn_to_stack_depth (Builtins.arity b)

	and write_expr(Ast.Expr(loc, kind) as expr): unit =
		match kind with
		| Ast.Access _ ->
			begin match Bind.value_binding bindings expr with
			| Binding.Builtin b ->
				write_bc (Code.Const (Builtins.value b))
			| Binding.Declared _ ->
				raise U.TODO
			| Binding.Local l ->
				(* stack_depth points to the *next* thing on the stack, so
					we subtract 1 from the difference.
					e.g. If we push a value and immediately access it, diff should be 0. *)
				let diff = !stack_depth - (get_local_depth l) - 1 in
				write_bc (Code.Load diff)
			| Binding.BuiltinType _ | Binding.DeclaredType _ ->
				CompileError.raise loc CompileError.CantUseTypeAsValue
			end;
			incr stack_depth

		| Ast.Call(called, args) ->
			let Ast.Expr(_, kind) = called in
			begin match kind with
			| Ast.Access _ ->
				begin match Bind.value_binding bindings called with
				| Binding.Builtin b ->
					write_builtin b args
				| Binding.Declared f ->
					Array.iter write_expr args;
					let fn = get_func f in
					write_bc (Code.Call fn);
					apply_fn_to_stack_depth (Code.func_arity fn)
				| Binding.Local _ ->
					raise U.TODO (* lambda *)
				| Binding.BuiltinType _ ->
					CompileError.raise loc CompileError.CantUseTypeAsValue
				| Binding.DeclaredType r ->
					Array.iter write_expr args;
					let record = TypeCheck.type_of_type_ast types r in
					write_bc (Code.Construct record);
					apply_fn_to_stack_depth (Type.record_arity record)
				end
			| _ ->
				raise U.TODO
			end

		| Ast.Let(declare, value, expr) ->
			(* Remember the depth of the value. *)
			set_local_depth declare;
			write_expr value;
			write_expr expr;
			write_bc Code.UnLet;
			decr stack_depth

		| Ast.Literal value ->
			write_bc (Code.Const value);
			incr stack_depth

		| Ast.Seq(a, b) ->
			write_expr a;
			write_bc Code.Drop;
			write_expr b in

	let write_param p =
		set_local_depth p;
		incr stack_depth in
	Array.iter write_param params;

	write_expr e;

	let drop_param p =
		write_bc (Code.UnLet);
		decr stack_depth in
	Array.iter drop_param params;

	U.assert_equal OutputU.output_int !stack_depth 1;
	write_bc Code.Return;
	BatDynArray.to_array code

let write_modul(Ast.Modul(_, decls))(bindings: Bind.t)(types: TypeCheck.t): Modul.t =
	(*
	Funcs may recursively depend upon each other.
	So, initalize them all now once, and then write to each func's code.
	*)
	let funcs: (Ast.decl_val, Code.func) Hashtbl.t = Hashtbl.create 0 in
	let get_func f =
		(*TODO:lookup*)
		Hashtbl.find funcs f in
	let get_fn = function
	| Ast.Val dv ->
		Some dv
	| _ ->
		None in
	let fns = BatArray.filter_map get_fn decls in

	let set_fn f =
		Hashtbl.add funcs f (Code.empty_func_from_ast f) in
	Array.iter set_fn fns;

	let write_fn_code(Ast.DeclVal(_, _, Ast.Fn(Ast.Signature(_, _, params), body)) as f): Code.func =
		let code = write_code get_func bindings types params body in
		U.returning (get_func f) (fun func -> func.Code.code <- code) in

	let code_fns = Array.map write_fn_code fns in

	{ Modul.recs = TypeCheck.all_records types; Modul.fns = code_fns }
