val unexpected: Loc.pos -> Lexer.t -> Token.t -> 'a
val must_skip: Lexer.t -> Token.t -> unit
val parse_name: Lexer.t -> Symbol.t
val parse_type_name: Lexer.t -> Symbol.t
