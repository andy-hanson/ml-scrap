type t
val f: (Ast.access -> Binding.t) -> TypeOfAst.t -> Ast.fn array -> t
val type_of_expr: t -> Ast.expr -> Type.t
val type_of_fn: t -> Ast.fn -> Type.ft
val type_of_local: t -> Ast.local_declare -> Type.t
val type_of_parameter: t -> Ast.parameter -> Type.t
