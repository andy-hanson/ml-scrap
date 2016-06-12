val builtin_name: Type.t -> string

val fn: Type.t -> Type.t array -> Type.fn
val t_fn: Type.t -> Type.t array -> Type.t

val rc_arity: Type.rc -> int

val subsumes: Type.t -> Val.t -> bool
val check_subsumes: Type.t -> Val.t -> unit (* This is for when you're not sure *)
val assert_subsumes: Type.t -> Val.t -> unit (* This is for when you are *)

val output_property: (Type.property, 'o) OutputU.printer
val output_fn: (Type.fn, 'o) OutputU.printer
val output_rc: (Type.rc, 'o) OutputU.printer
val output: (Type.t, 'o) OutputU.printer
