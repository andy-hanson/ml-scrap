type pos = int
type t

val make: pos -> pos -> t
val single: pos -> t

type lc_pos = { line: int; column: int }
type lc_loc = { lc_start: lc_pos; lc_rear: lc_pos }

val lc_pos: BatIO.input -> pos -> lc_pos
val lc_loc: BatIO.input -> t -> lc_loc

val output_pos: 'a OutputU.t -> pos -> unit
val output: 'a OutputU.t -> t -> unit

val output_lc_pos: 'a OutputU.t -> lc_pos -> unit
val output_lc_loc: 'a OutputU.t -> lc_loc -> unit
