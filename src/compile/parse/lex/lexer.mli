type t

val make: BatIO.input -> t

val pos: t -> Loc.pos

val loc_from: t -> Loc.pos -> Loc.t

val next: t -> CompileContext.t -> Token.t
