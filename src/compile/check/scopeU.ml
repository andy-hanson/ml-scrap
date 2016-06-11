let get(scope: Scope.t)(loc: Loc.t)(name: Symbol.t): Binding.t =
	try
		Symbol.Map.find name scope
	with Not_found ->
		CompileError.raise loc (CompileError.CantBind name)

let add(scope: Scope.t)(loc: Loc.t)(name: Symbol.t)(binding: Binding.t): Scope.t =
	match Symbol.Map.try_get scope name with
	| Some old_binding ->
		CompileError.raise loc (CompileError.NameAlreadyBound(name, old_binding))
	| None ->
		Symbol.Map.add name binding scope

let add_local(scope: Scope.t)(Ast.LocalDeclare(loc, name) as local): Scope.t =
	add scope loc name (Binding.Local local)

let add_params(scope: Scope.t)(params: Ast.parameter array): Scope.t =
	ArrayU.fold scope params begin fun scope (Ast.Parameter(loc, name, _) as parameter) ->
		add scope loc name (Binding.Parameter parameter)
	end

let get_base(ctx: CompileContext.t)(decls: Ast.decl array): Scope.t =
	let builtins = CompileContext.builtins_scope ctx in
	ArrayU.fold builtins decls begin fun scope decl ->
		let loc, name = Ast.decl_loc_name decl in
		add scope loc name (Binding.Declared decl)
	end
