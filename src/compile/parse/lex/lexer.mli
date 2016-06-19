type t

val make: (Loc.t -> CompileError.message -> unit) -> BatIO.input -> t

val pos: t -> Loc.pos
val pos_next: t -> Loc.pos * Token.t
val loc_from: t -> Loc.pos -> Loc.t

val next: t -> Token.t
