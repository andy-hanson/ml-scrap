type t
val build: FileIO.path -> Bind.t -> Ast.modul -> N.modul * t
val rt_of_ast: t -> Ast.rt -> N.rt
val val_of_ast: t -> Ast.decl_val -> N.v
val ty_of_ast: t -> Ast.decl_ty -> N.ty
(*TODO:KILL val ty_or_v_of_ast: t -> Ast.decl -> N.ty_or_v*)
val fn_of_ast: t -> Ast.fn -> N.declared_fn
val ft_of_fn: t -> Ast.fn -> N.ft
val ft_of_ast: t -> Ast.ft -> N.ft
val parameter_ty: t -> Ast.parameter -> N.ty
val declared_ty: Bind.t -> t -> Ast.ty -> N.ty

val output: (t, 'o) OutputU.printer
