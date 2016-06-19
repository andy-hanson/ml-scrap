val expect: Loc.pos -> Lexer.t -> Token.t -> Token.t -> unit
val unexpected: Loc.pos -> Lexer.t -> Token.t -> 'a
val must_skip: Lexer.t -> Token.t -> unit
val parse_name: Lexer.t -> Sym.t
val parse_type_name: Lexer.t -> Sym.t
