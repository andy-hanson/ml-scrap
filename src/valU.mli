open Val

val equal: t -> t -> bool

val type_of: t -> Type.t

exception CastFail

val bool_of: t -> bool
val int_of: t -> int
val float_of: t -> float

val fn_info_name: fn_info -> Sym.t
val fn_name: fn -> Sym.t
val builtin_fn_name: builtin_fn -> Sym.t
val fn_arity: fn -> int

val output: (t, 'o) OutputU.printer

val output_fn: (fn, 'o) OutputU.printer
val output_bytecode: (bytecode, 'o) OutputU.printer
val output_code: (code, 'o) OutputU.printer
