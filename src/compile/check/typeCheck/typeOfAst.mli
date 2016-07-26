open N.V
open N.Ty
open N.Compiler

type t
val build: Path.t -> Path.t -> Bind.t -> Ast.modul -> modul * t
val rt_of_ast: t -> Ast.rt -> rt
val val_of_ast: t -> Ast.decl_val -> v
val fn_of_ast: t -> Ast.fn -> declared_fn
val parameter_ty: t -> Ast.parameter -> ty

(*TODO: just have TypeOfAst store the bindings, so they don't have to be passed in*)
val declared_ty: Bind.t -> t -> Ast.ty -> ty
val instantiate_generic: Bind.t -> t -> Loc.t -> ty -> Ast.ty array -> ty

val output: (t, 'o) OutputU.printer
