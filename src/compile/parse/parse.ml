let parse(ctx: CompileContext.t)(source: BatIO.input): Ast.modul =
	let parser = Parser.make ctx source in
	ParseModule.parse_module parser
