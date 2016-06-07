let f(ctx: CompileContext.t)(source: BatIO.input): Ast.modul =
	ParseModule.parse_module (Lexer.make ctx source)
