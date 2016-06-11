type t
val bind: CompileContext.t -> Ast.modul -> t
(*TODO: would be nice if these were the same function (Ast.access type?)*)
val value_binding: t -> Ast.expr -> Binding.t
val type_binding: t -> Ast.typ -> Binding.t
val output: 'o OutputU.t -> t -> unit
