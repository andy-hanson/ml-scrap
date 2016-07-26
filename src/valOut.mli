open N.V

val output: (v, 'o) OutputU.printer
val output_primitive: (primitive, 'o) OutputU.printer

val output_declared_fn: (declared_fn, 'o) OutputU.printer
(*TODO: bytecodes are not values, move these functions!*)
val output_bytecode: (N.Code.bytecode, 'o) OutputU.printer
val output_code: (N.Code.code, 'o) OutputU.printer

val to_string: v -> string
