open N

val create: FileIo.t -> compiler

val lex: compiler -> Path.t -> (Token.t * Loc.t) array
val parse: compiler -> Path.t -> Ast.modul
val compile: compiler -> Path.t -> modul

val lc_loc: compiler -> Path.t -> Loc.t -> Loc.lc_loc
