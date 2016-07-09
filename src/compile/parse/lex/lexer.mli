type t

val make: BatIO.input -> t

val pos: t -> Loc.pos
val pos_next: t -> Loc.pos * Token.t
val loc_from: t -> Loc.pos -> Loc.t

val next: t -> Token.t
(* true: done; false: more interpolation to see *)
val next_quote_part: t -> string * bool
