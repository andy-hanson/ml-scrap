open N.V
open N.Ty

val v_bool: bool -> v
val v_float: float -> v
val v_int: int -> v
val v_string: string -> v
val v_void: v

val equal: v -> v -> bool

val ty_of: v -> ty
val ty_of_primitive: primitive -> ty_primitive

exception CastFail

val bool_of: v -> bool
val int_of: v -> int
val float_of: v -> float

val fn_name: declared_fn -> Sym.t
val builtin_fn_name: builtin_fn -> Sym.t
val fn_arity: declared_fn -> int
