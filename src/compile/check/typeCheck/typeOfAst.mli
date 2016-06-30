type t
val build: FileIO.path -> (Ast.access -> Binding.t) -> Ast.modul -> N.modul * t
val rt_of_ast: t -> Ast.rt -> N.rt
val type_of_ast: t -> Ast.decl -> N.ty
val ty_or_v_of_ast: t -> Ast.decl -> N.ty_or_v
val fn_of_ast: t -> Ast.fn -> N.declared_fn
val ft_of_ast: t -> Ast.ft -> N.ft
val ct_of_ast: t -> Ast.ct -> N.ct
val parameter_type: t -> Ast.parameter -> N.ty
(*TODO:RENAME!*)
val cn_of_ast: t -> Ast.cn -> N.declared_fn
val declared_type: (Ast.access -> Binding.t) -> t -> Ast.typ -> N.ty

val output: (t, 'o) OutputU.printer
