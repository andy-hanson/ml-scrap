type t

val make: CompileContext.t -> BatIO.input -> t

val next: t -> Token.t

val pos: t -> Loc.pos

val loc_from: t -> Loc.pos -> Loc.t

val loc_at: t -> Loc.t
