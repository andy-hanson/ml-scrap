open N.Ty

type t
val f: Bind.t -> TypeOfAst.t -> Ast.modul -> t
val ty_of_expr: t -> Ast.expr -> ty
val ty_of_local: t -> Ast.local_declare -> ty
