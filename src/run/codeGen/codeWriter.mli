open N.V
open N.Ty
open N.Code

type t

val write: Bind.t -> TypeOfAst.t -> TypeCheck.t -> Ast.parameter array -> Loc.t -> (t -> unit) -> code

val apply_fn_to_stack_depth: t -> int -> unit

(*TODO: rename to register_local or something*)
val set_local_depth: t -> Ast.local_declare -> unit
val incr_stack_depth: t -> unit
val decr_stack_depth: t -> unit

(*TODO:move down*)
val access_local: t -> Loc.t -> Ast.local_declare -> unit
val access_parameter: t -> Loc.t -> Ast.parameter -> unit

val bindings: t -> Bind.t
val type_of_ast: t -> TypeOfAst.t
val tys: t -> TypeCheck.t

(*TODO: ordering*)
val dup: t -> Loc.t -> unit
val un_let: t -> Loc.t -> int -> unit
val const: t -> Loc.t -> v -> unit
val drop: t -> Loc.t -> unit
val call: t -> Loc.t -> int -> unit
val partial: t -> Loc.t -> int -> unit
val quote: t -> Loc.t -> string array -> unit
val check: t -> Loc.t -> unit
val destruct: t -> Loc.t -> pattern array -> unit
val get_property: t -> Loc.t -> int -> unit
val cnv_rc: t -> Loc.t -> rt -> int array -> unit

type placeholder
val placeholder: t -> Loc.t -> placeholder
val resolve_goto: t -> placeholder -> unit
val resolve_goto_if_false: t -> placeholder -> unit

type cases
val cs: t -> Loc.t -> int -> cases
val resolve_cs_part: t -> cases -> int -> ty -> unit
