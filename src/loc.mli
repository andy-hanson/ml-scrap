type pos = int
(* Stores start and rear positions. *)
type t

val zero: t

val start: t -> pos
val rear: t -> t

val make: pos -> pos -> t
val single_character: pos -> t

val hash: t -> int


(* We store locs compactly as index pairs and need the source file to recover line/column. *)

type lc_pos = {line: int; column: int}
type lc_loc = {lc_start: lc_pos; lc_rear: lc_pos}

val lc_pos: BatIO.input -> pos -> lc_pos
val lc_loc: string -> t -> lc_loc

val output_pos: 'o OutputU.t -> pos -> unit
val output: 'o OutputU.t -> t -> unit

val output_lc_pos: 'o OutputU.t -> lc_pos -> unit
val output_lc_loc: 'o OutputU.t -> lc_loc -> unit
