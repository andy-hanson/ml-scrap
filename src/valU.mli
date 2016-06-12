open Val

val typ: t -> Type.t

exception CastFail

val bool_of: t -> bool
val int_of: t -> int
val float_of: t -> float

val output: (t, 'o) OutputU.printer
