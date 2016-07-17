open Scope

val get_v: t -> Loc.t -> Sym.t -> Binding.v
val get_ty: t -> Loc.t -> Sym.t -> Binding.ty
val get_base: (Path.rel -> N.modul) -> Ast.imports array -> Ast.decl array -> Scope.t
val add_local: t -> Ast.local_declare -> t
val add_params: t -> Ast.parameter array -> t
