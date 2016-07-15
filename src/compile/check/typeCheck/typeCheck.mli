type t
val f: Bind.t -> TypeOfAst.t -> Ast.modul -> t
val ty_of_expr: t -> Ast.expr -> N.ty
val ty_of_local: t -> Ast.local_declare -> N.ty
