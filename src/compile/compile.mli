open N.Compiler

val lex: BatIO.input -> (Token.t * Loc.t) array
val check_and_generate: (Path.rel -> modul) -> Path.t -> Path.t -> Ast.modul -> modul
