let declared_type(binding: Ast.access -> Binding.t)(type_of_ast: TypeOfAst.t): Ast.typ -> Type.t =
	let rec recur(typ: Ast.typ): Type.t =
		match typ with
		| Ast.TypeAccess(access) ->
			begin match binding access with
			| Binding.Builtin _ | Binding.Local _ | Binding.Parameter _ ->
				raise U.TODO (*TODO: not-a-type error*)
			| Binding.BuiltinType b ->
				b
			| Binding.Declared d ->
				(*TODO: helper in TypeOfAst.ml for doing this*)
				begin match d with
				| Ast.DeclRc r ->
					Type.Rc (TypeOfAst.rc_of_ast type_of_ast r)
				| Ast.DeclUn u ->
					Type.Un (TypeOfAst.un_of_ast type_of_ast u)
				| Ast.DeclFt f ->
					Type.Ft (TypeOfAst.ft_of_ast type_of_ast f)
				| Ast.DeclFn _ ->
					assert false
				end
			end
		| Ast.TypeFn(_, return_type, parts) ->
			(*TODO: no anonymous types!*)
			let parameters = ArrayU.map parts (fun part -> Sym.of_string "anonymous", recur part) in
			Type.Ft(TypeU.ft (Sym.of_string "anonymous") (recur return_type) parameters) in
	recur

let combine_types(loc: Loc.t)(types: Type.t array): Type.t =
	(*TODO: actual union algorithm*)
	let t = Array.get types 0 in
	ArrayU.iter types begin fun typ ->
		CompileErrorU.check (t = typ) loc (CompileError.CombineTypes(t, typ))
	end;
	t
