type pos
type t

val start_pos: pos

val pos: int -> int -> pos
val line: pos -> int
val column: pos -> int

val make: pos -> pos -> t
val single: pos -> t

(*TODO:KILL THESE*)
val next_line: pos -> pos
val prev_line: pos -> pos
val next_column: pos -> pos

val output_pos: 'a OutputU.t -> pos -> unit
val output: 'a OutputU.t -> t -> unit
