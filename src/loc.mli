type pos
type t

val start_pos: pos

val make: pos -> pos -> t
val single: pos -> t

val line: pos -> int
val next_line: pos -> pos
val prev_line: pos -> pos
val next_column: pos -> pos

val output_pos: 'a OutputU.t -> pos -> unit
val output: 'a OutputU.t -> t -> unit
