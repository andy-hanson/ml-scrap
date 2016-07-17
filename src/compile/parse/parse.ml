let f(source: BatIO.input): Ast.modul =
	ParseModul.f @@ Lexer.make source
