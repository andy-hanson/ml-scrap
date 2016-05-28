let parse(ctx: CompileContext.t)(source: string): Ast.modul =
	let parser = Parser.make ctx source in
	ParseModule.parse_module parser
