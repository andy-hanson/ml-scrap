type t
val bind: CompileContext.t -> Ast.modul -> t
val binding: t -> Ast.access -> Binding.t
val output: (t, 'o) OutputU.printer
