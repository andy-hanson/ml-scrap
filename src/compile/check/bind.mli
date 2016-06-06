type t

val bind: CompileContext.t -> Ast.modul -> t

val value_binding: t -> Ast.expr -> Binding.t
val type_binding: t -> Ast.typ -> Binding.t

(* val output: 'a OutputU.t -> bindings -> unit *)
