type t

val make: CompileContext.t -> string -> t

val next: t -> Token.t

val pos: t -> Loc.pos

val loc_from: t -> Loc.pos -> Loc.t

val loc_at: t -> Loc.t

val with_restore: t -> (unit -> 'a) -> 'a
