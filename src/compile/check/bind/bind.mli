type t
val bind: Ast.decl array -> t
val binding: t -> Ast.access -> Binding.t
val output: (t, 'o) OutputU.printer
