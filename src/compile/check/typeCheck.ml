(*TODO: lots of cleanup around here!*)

(*TODO:RENAME to ExprTypes*)
module Types = Lookup.T(struct type t = Ast.expr end)
type types = Type.t Types.t

module FnTypes = Lookup.T(struct type t = Ast.decl_val end)
type fn_types = Type.t FnTypes.t

module Records = Lookup.T(struct type t = Ast.decl_type end)
type records = Type.record Records.t

type t = {
	types: types;
	fn_types: fn_types;
	records: records
}

let type_of_expr({types}: t)(expr: Ast.expr): Type.t =
	Types.get types expr
let type_of_fn({fn_types}: t)(fn: Ast.decl_val): Type.t =
	FnTypes.get fn_types fn
let type_of_type_ast({records}: t)(type_ast: Ast.decl_type): Type.record =
	Records.get records type_ast
let all_records({records}: t): Type.record array =
	Records.values records


(*TODO:MOVE?*)
let empty_rec_from_ast(Ast.DeclType(_, name, Ast.Rec(props))): Type.record =
	{ Type.rname = name; Type.properties = [||] }

let declared_type(bindings: Bind.t)(records: records)(Ast.TypeAccess(loc, name) as typ: Ast.typ): Type.t =
	match Bind.type_binding bindings typ with
	| Binding.Builtin _ | Binding.Declared _ | Binding.Local _ ->
		raise U.TODO (*TODO: not-a-type error*)
	| Binding.BuiltinType b ->
		Type.Builtin b
	| Binding.DeclaredType d ->
		Type.Rec (Records.get records d)

(*TODO: close over ctx and bindings*)
let get_records(ctx: CompileContext.t)(bindings: Bind.t)(decls: Ast.decl array): records =
	U.returning (Records.create()) begin fun records ->
		let record_asts = BatArray.filter_map (function | Ast.Type dt -> Some dt | _ -> None) decls in
		Array.iter (fun r -> Records.set records r (empty_rec_from_ast r)) record_asts;

		let write_rec(Ast.DeclType(_, _, Ast.Rec(properties)) as record_ast): unit =
			let record = Records.get records record_ast in
			let make_prop(Ast.Property(_, name, typ)) =
				{ Type.prop_name = name; Type.prop_type = declared_type bindings records typ } in
			record.Type.properties <- Array.map make_prop properties in
		Array.iter write_rec record_asts
	end

let f(ctx: CompileContext.t)(Ast.Modul(_, decls))(bindings: Bind.t): t =
	let records = get_records ctx bindings decls in
	let declared_type = declared_type bindings records in

	let types = Types.create() in

	let assert_assignable(a: Type.t)(b: Type.t)(loc: Loc.t): unit =
		(*TODO: supertypes, generics, and whatnot*)
		CompileError.check (a = b) loc (CompileError.NotExpectedType(a, b)) in

	let declaration_type(d: Ast.decl_val): Type.t =
		raise U.TODO in

	let rec assert_type(expected: Type.t)(Ast.Expr(loc, _) as expr): unit =
		let actual = check_expr expr in
		assert_assignable expected actual loc

	and check_expr(Ast.Expr(loc, kind) as expr) =
		let expr_type = match kind with
			| Ast.Access _ ->
				begin match Bind.value_binding bindings expr  with
				| Binding.Builtin b ->
					Builtins.type_of b
				| Binding.Declared d ->
					declaration_type d
				| Binding.Local(Ast.LocalDeclare(_, _, typ)) ->
					(*TODO: just set the type once at declaration and reuse it*)
					declared_type typ
				| Binding.BuiltinType _ ->
					raise U.TODO (*TODO: some appropriate error*)
				| Binding.DeclaredType d ->
					(*TODO: constructor, so a function type*)
					let r = Records.get records d in
					let params = Array.map (fun p -> p.Type.prop_type) r.Type.properties in
					Type.Fn(Type.Rec r, params)
				end

			| Ast.Call(called, args) ->
				let called_type = check_expr called in
				let return_type, param_types = match called_type with
					| Type.Fn(r, a) ->
						r, a
						| _ ->
						CompileError.raise loc CompileError.NotCallable in
				let n_params = Array.length param_types in
				let n_args = Array.length args in
				CompileError.check (n_params = n_args) loc (CompileError.NumArgs(n_params, n_args));
				U.iter_zip param_types args begin fun typ arg ->
					assert_type typ arg
				end;
				return_type

			| Ast.Let(Ast.LocalDeclare(_, _, type_ast), value, expr) ->
				let typ = declared_type type_ast in
				assert_type typ value;
				check_expr expr

			| Ast.Literal v ->
				Val.typ v

			| Ast.Seq(a, b) ->
				assert_type (Type.Builtin Type.Void) a;
				check_expr b in
		Types.set types expr expr_type;
		expr_type in

	let fn_type(Ast.Fn(Ast.Signature(_, return_type_ast, params), body)): Type.t =
		let param_type(Ast.LocalDeclare(_, _, typ)) =
		 declared_type typ in
		let param_types = Array.map param_type params in
		let return_type = declared_type return_type_ast in
		assert_type return_type body;
		Type.Fn(return_type, param_types) in

	let fn_types = FnTypes.create() in

	(* type context stuff here *)
	let check_decl = function
		| Ast.Val(Ast.DeclVal(_, _, fn) as decl) ->
			FnTypes.set fn_types decl (fn_type fn)
		| Ast.Type _ ->
			(* Done in get_records *)
			(*TODO: only iterate over module once, and split*)
			() in
	Array.iter check_decl decls;

	{ types; fn_types; records }
