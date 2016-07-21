type t

val create: FileIo.t -> t

val lex: t -> Path.t -> (Token.t * Loc.t) array Lwt.t
val parse: t -> Path.t -> Ast.modul Lwt.t
val compile: t -> Path.t -> N.modul Lwt.t

val lc_loc: t -> Path.t -> Loc.t -> Loc.lc_loc Lwt.t
