type t

val create: FileIO.t -> t

val lex: t -> FileIO.path -> (Token.t * Loc.t) array
val parse: t -> FileIO.path -> Ast.modul
val compile: t -> FileIO.path -> N.modul

val lc_loc: t -> FileIO.path -> Loc.t -> Loc.lc_loc
