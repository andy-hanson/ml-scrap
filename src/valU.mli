open N

val equal: v -> v -> bool

val type_of: v -> ty

exception CastFail

val bool_of: v -> bool
val int_of: v -> int
val float_of: v -> float

val fn_name: fn -> Sym.t
val builtin_fn_name: builtin_fn -> Sym.t
val fn_arity: fn -> int
(*val cn_name: cn -> Sym.t*)

val output: (v, 'o) OutputU.printer

val output_fn: (fn, 'o) OutputU.printer
val output_bytecode: (bytecode, 'o) OutputU.printer
val output_code: (code, 'o) OutputU.printer
