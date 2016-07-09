let f(source: BatIO.input): Ast.modul =
	ParseModule.f @@ Lexer.make source
