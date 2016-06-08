(*TODO: lots of cleanup around here!*)

(*TODO:RENAME to ExprTypes*)
module Types = Lookup.T(struct type t = Ast.expr end)
type types = Type.t Types.t

module FnTypes = Lookup.T(struct type t = Ast.decl_val end)
type fn_types = Type.fn FnTypes.t

module Records = Lookup.T(struct type t = Ast.decl_type end)
type records = Type.record Records.t

(*TODO: these are not used as output so putting them here is misleading*)
module Locals = Lookup.T(struct type t = Ast.local_declare end)
type locals = Type.t Locals.t


type t = {
	types: types;
	fn_types: fn_types;
	records: records
}

let type_of_expr({types; _}: t)(expr: Ast.expr): Type.t =
	Types.get types expr
let type_of_fn({fn_types; _}: t)(fn: Ast.decl_val): Type.fn =
	FnTypes.get fn_types fn
let type_of_type_ast({records; _}: t)(type_ast: Ast.decl_type): Type.record =
	Records.get records type_ast
let all_records({records; _}: t): Type.record array =
	Records.values records


(*TODO:MOVE?*)
let empty_rec_from_ast(Ast.DeclType(_, name, _)): Type.record =
	{ Type.rname = name; Type.properties = [||] }

let declared_type(bindings: Bind.t)(records: records)(typ: Ast.typ): Type.t =
	match Bind.type_binding bindings typ with
	| Binding.Builtin _ | Binding.Declared _ | Binding.Local _ | Binding.Parameter _ ->
		raise U.TODO (*TODO: not-a-type error*)
	| Binding.BuiltinType b ->
		b
	| Binding.DeclaredType d ->
		Type.Rec (Records.get records d)

let get_records(bindings: Bind.t)(decls: Ast.decl array): records =
	U.returning (Records.create()) begin fun records ->
		let record_asts = BatArray.filter_map (function | Ast.Type dt -> Some dt | _ -> None) decls in
		ArrayU.iter record_asts begin fun r ->
			Records.set records r (empty_rec_from_ast r)
		end;

		ArrayU.iter record_asts begin fun (Ast.DeclType(_, _, Ast.Rec(properties)) as record_ast) ->
			let record = Records.get records record_ast in
			record.Type.properties <- ArrayU.map properties begin fun (Ast.Property(_, name, typ)) ->
				{ Type.prop_name = name; Type.prop_type = declared_type bindings records typ }
			end
		end
	end

(*TODO: major cleanup!*)

let f(Ast.Modul(_, decls))(bindings: Bind.t): t =
	let records = get_records bindings decls in
	let declared_type = declared_type bindings records in

	let types: types = Types.create() in
	let locals: locals = Locals.create() in

	let fn_type(Ast.Fn(Ast.Signature(_, return_type_ast, params), _)): Type.fn =
		let param_types = ArrayU.map params begin fun (Ast.Parameter(_, _, typ)) ->
			declared_type typ
		end in
		let return_type = declared_type return_type_ast in
		Type.fn return_type param_types in

	let fn_types = FnTypes.create() in
	(*TODO:neater*)
	ArrayU.iter decls begin function
		| Ast.Val(Ast.DeclVal(_, _, fn) as decl) ->
			FnTypes.set fn_types decl (fn_type fn)
		| Ast.Type _ -> ()
	end;

	let assert_assignable(a: Type.t)(b: Type.t)(loc: Loc.t): unit =
		(*TODO: supertypes, generics, and whatnot*)
		CompileError.check (a = b) loc (CompileError.NotExpectedType(a, b)) in


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
					Type.Fn (FnTypes.get fn_types d)
				| Binding.Local(declare) ->
					Locals.get locals declare
				| Binding.Parameter(Ast.Parameter(_, _, typ)) ->
					declared_type typ
				| Binding.BuiltinType _ ->
					raise U.TODO (*TODO: some appropriate error*)
				| Binding.DeclaredType d ->
					let r = Records.get records d in
					let params = ArrayU.map r.Type.properties (fun p -> p.Type.prop_type) in
					Type.t_fn (Type.Rec r) params
				end

			| Ast.Call(called, args) ->
				let called_type = check_expr called in
				let return_type, param_types = match called_type with
					| Type.Fn {Type.return_type; Type.parameters} ->
						return_type, parameters
					| _ ->
						CompileError.raise loc CompileError.NotCallable in
				let n_params = Array.length param_types in
				let n_args = Array.length args in
				CompileError.check (n_params = n_args) loc (CompileError.NumArgs(n_params, n_args));
				ArrayU.iter_zip param_types args begin fun typ arg ->
					assert_type typ arg
				end;
				return_type

			| Ast.Let(declare, value, expr) ->
				let typ = check_expr value in
				Locals.set locals declare typ;
				check_expr expr

			| Ast.Literal v ->
				Val.typ v

			| Ast.Seq(a, b) ->
				assert_type Type.Void a;
				check_expr b in
		Types.set types expr expr_type;
		expr_type in

	(* type context stuff here *)
	ArrayU.iter decls begin function
		| Ast.Val(Ast.DeclVal(_, _, Ast.Fn(_, body)) as decl) ->
			let foo: Type.fn = FnTypes.get fn_types decl in
			let t: Type.t = foo.Type.return_type in
			assert_type t body
		| Ast.Type _ ->
			(* Done in get_records *)
			(*TODO: only iterate over module once, and split*)
			()
	end;

	{ types; fn_types; records }
