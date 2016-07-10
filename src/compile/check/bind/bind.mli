type t
val bind: Ast.modul -> t
(*TODO: it's easy to forget which of these to call...*)
val binding: t -> Ast.access -> Binding.v
val ty_binding: t -> Ast.access -> Binding.ty
val output: (t, 'o) OutputU.printer
