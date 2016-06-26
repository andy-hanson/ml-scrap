type t
val build: FileIO.file_name -> (Ast.access -> Binding.t) -> Ast.modul -> N.modul * t
val rt_of_ast: t -> Ast.rt -> N.rt
val type_of_ast: t -> Ast.decl -> N.ty
val fn_of_ast: t -> Ast.fn -> N.fn
val parameter_type: t -> Ast.parameter -> N.ty
(*TODO:RENAME!*)
val cn_of_ast: t -> Ast.cn -> N.fn
val declared_type: (Ast.access -> Binding.t) -> t -> Ast.typ -> N.ty

val output: (t, 'o) OutputU.printer
