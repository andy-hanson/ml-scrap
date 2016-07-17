val lex: FileIo.t -> Path.t -> (Token.t * Loc.t) array
val check_and_generate: (Path.rel -> N.modul) -> Path.t -> Path.t -> Ast.modul -> N.modul
