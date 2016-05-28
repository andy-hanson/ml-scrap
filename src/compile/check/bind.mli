type bindings

val bind: CompileContext.t -> Ast.modul -> bindings

val get_binding: bindings -> Ast.expr -> Binding.t

(* val output: 'a BatIO.output -> bindings -> unit *)
