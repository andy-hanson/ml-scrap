type t

val create: FileIo.t -> t

val lex: t -> Path.t -> (Token.t * Loc.t) array
val parse: t -> Path.t -> Ast.modul
val compile: t -> Path.t -> N.modul

val lc_loc: t -> Path.t -> Loc.t -> Loc.lc_loc
