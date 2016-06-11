type t

val make: CompileContext.t -> BatIO.input -> t

(*TODO:DOC: these are used as a pair*)
val pos: t -> Loc.pos
val loc_from: t -> Loc.pos -> Loc.t

val next: t -> Token.t
