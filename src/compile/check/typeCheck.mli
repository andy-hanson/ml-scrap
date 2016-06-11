type t
val f: Ast.modul -> Bind.t -> t
val type_of_expr: t -> Ast.expr -> Type.t
val type_of_fn: t -> Ast.fn -> Type.fn
val rc_of_ast: t -> Ast.rc -> Type.rc
val all_rcs: t -> Type.rc array
