type t
val f: (Ast.access -> Binding.t) -> TypeOfAst.t -> Ast.modul -> t
val type_of_expr: t -> Ast.expr -> N.ty
val type_of_local: t -> Ast.local_declare -> N.ty
