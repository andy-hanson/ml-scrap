open Builtin

val name: t -> string
val type_of: t -> Type.t
val value: t -> Val.t
val arity: t -> int
val output: (t, 'o) OutputU.printer
