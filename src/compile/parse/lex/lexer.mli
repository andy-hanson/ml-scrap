type t

val make: string -> t

val pos: t -> Loc.pos

val loc_from: t -> Loc.pos -> Loc.t

val next: t -> CompileContext.t -> Token.t

type restore
val get_restore: t -> restore
val do_restore: t -> restore -> unit
