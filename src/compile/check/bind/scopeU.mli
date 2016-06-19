val get: Scope.t -> Loc.t -> Sym.t -> Binding.t
val get_base: Ast.decl array -> Scope.t
val add_local: Scope.t -> Ast.local_declare -> Scope.t
val add_params: Scope.t -> Ast.parameter array -> Scope.t
