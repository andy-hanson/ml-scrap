let get(scope: Scope.t)(loc: Loc.t)(name: Sym.t): Binding.t =
	try
		Sym.Map.find name scope
	with Not_found ->
		CompileErrorU.raise loc (CompileError.CantBind name)

let add(scope: Scope.t)(loc: Loc.t)(name: Sym.t)(binding: Binding.t): Scope.t =
	match Sym.Map.try_get scope name with
	| Some old_binding ->
		CompileErrorU.raise loc (CompileError.NameAlreadyBound(name, old_binding))
	| None ->
		Sym.Map.add name binding scope

let add_local(scope: Scope.t)(Ast.LocalDeclare(loc, name) as local): Scope.t =
	add scope loc name (Binding.Local local)

let add_params(scope: Scope.t)(params: Ast.parameter array): Scope.t =
	ArrayU.fold scope params begin fun scope (Ast.Parameter(loc, name, _) as parameter) ->
		add scope loc name (Binding.Parameter parameter)
	end

let builtins =
		(*TODO: just have Binding.Builtin and a single map here...*)
		let m1 = Sym.Map.make Builtin.all begin fun ({Builtin.name; _} as b) ->
			name, Binding.Builtin b
		end in
		let m2 = Sym.Map.make BuiltinType.all begin fun b ->
			TypeU.name b, Binding.BuiltinType b
		end in
		(* There are no shared names *)
		Sym.Map.union (fun _ _ _ -> assert false) m1 m2

let get_base(decls: Ast.decl array): Scope.t =
	ArrayU.fold builtins decls begin fun scope decl ->
		let loc, name = AstU.decl_loc_name decl in
		add scope loc name (Binding.Declared decl)
	end