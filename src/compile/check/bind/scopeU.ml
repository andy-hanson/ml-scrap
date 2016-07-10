open Scope

let get_v({vals; _}: t)(loc: Loc.t)(name: Sym.t): Binding.v =
	try
		Sym.Map.find name vals
	with Not_found ->
		ErrU.raise loc @@ Err.CantBind name

let get_ty({tys; _}: t)(loc: Loc.t)(name: Sym.t): Binding.ty =
	(*TODO: duplicate code*)
	try
		Sym.Map.find name tys
	with Not_found ->
		ErrU.raise loc @@ Err.CantBind name

let add_v({vals; tys}: t)(loc: Loc.t)(name: Sym.t)(binding: Binding.v): t =
	match Sym.Map.try_get vals name with
	| Some old_binding ->
		ErrU.raise loc @@ Err.NameAlreadyBound(name, old_binding)
	| None ->
		{vals = Sym.Map.add name binding vals; tys}

let add_ty({vals; tys}: t)(loc: Loc.t)(name: Sym.t)(binding: Binding.ty): t =
	(*TODO: duplicate code*)
	match Sym.Map.try_get tys name with
	| Some old_binding ->
		ErrU.raise loc @@ Err.TypeNameAlreadyBound(name, old_binding)
	| None ->
		{vals; tys = Sym.Map.add name binding tys}

let add_local(scope: t)((loc, name) as local: Ast.local_declare): t =
	add_v scope loc name @@ Binding.Local local

let add_params(scope: t)(params: Ast.parameter array): t =
	ArrayU.fold scope params begin fun scope ((loc, name, _) as parameter) ->
		add_v scope loc name @@ Binding.Parameter parameter
	end

let builtins =
	{
		vals = Sym.Map.make Builtin.all (fun ({Builtin.name; value}) -> name, Binding.Builtin value);
		tys = Sym.Map.make BuiltinType.all (fun b -> TyU.name b, Binding.BuiltinType b)
	}

let get_base(decls: Ast.decl array): t =
	ArrayU.fold builtins decls begin fun scope decl ->
		let loc, name = AstU.decl_loc_name decl in
		match decl with
		| Ast.Fn _ | Ast.Cn _ ->
			add_v scope loc name @@ Binding.VDeclared decl
		| Ast.Rt _ | Ast.Un _ | Ast.Ft _ | Ast.Ct _ ->
			add_ty scope loc name @@ Binding.TDeclared decl
	end
