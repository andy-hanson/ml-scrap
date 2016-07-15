open N

val equal: v -> v -> bool

val ty_of: v -> ty
val ty_of_fn: fn -> ty_fn
val ty_of_primitive: primitive -> ty_primitive

exception CastFail

val bool_of: v -> bool
val int_of: v -> int
val float_of: v -> float

val fn_name: declared_fn -> Sym.t
val builtin_fn_name: builtin_fn -> Sym.t
val fn_arity: declared_fn -> int

val output: (v, 'o) OutputU.printer
val output_primitive: (primitive, 'o) OutputU.printer

val output_declared_fn: (declared_fn, 'o) OutputU.printer
val output_bytecode: (bytecode, 'o) OutputU.printer
val output_code: (code, 'o) OutputU.printer

val to_string: v -> string
