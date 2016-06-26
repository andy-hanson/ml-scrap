type t
val create: Bind.t -> TypeOfAst.t -> TypeCheck.t -> Ast.parameter array -> t
val apply_fn_to_stack_depth: t -> int -> unit

(*TODO: rename to register_local or something*)
val set_local_depth: t -> Ast.local_declare -> unit
val decr_stack_depth: t -> unit

val write_local_access: t -> Loc.t -> Ast.local_declare -> unit
val write_parameter_access: t -> Loc.t -> Ast.parameter -> unit

val finish: t -> Loc.t -> N.code

val binding: t -> Ast.access -> Binding.t
val fn_of_ast: t -> Ast.fn -> N.fn
val rt_of_ast: t -> Ast.rt -> N.rt
val type_of_local: t -> Ast.local_declare -> N.ty

val un_let: t -> Loc.t -> unit
val const: t -> Loc.t -> N.v -> unit
val drop: t -> Loc.t -> unit
val call_builtin: t -> Loc.t -> N.builtin_fn -> int -> unit
val call_static: t -> Loc.t -> N.fn -> int -> unit
val construct: t -> Loc.t -> N.rt -> int -> unit
val call_lambda: t -> Loc.t -> int -> unit

type placeholder
val placeholder: t -> Loc.t -> placeholder
val resolve_goto: t -> placeholder -> unit
val resolve_goto_if_false: t -> placeholder -> unit

type cases
val case: t -> Loc.t -> int -> cases
val resolve_case_part: t -> cases -> int -> N.ty -> unit
