type t
val f: Ast.modul -> Bind.t -> t
val type_of_expr: t -> Ast.expr -> Type.t
val type_of_fn: t -> Ast.decl_val -> Type.fn
val type_of_type_ast: t -> Ast.decl_type -> Type.record
val all_records: t -> Type.record array
