let parse_module(l: Lexer.t): Ast.modul =
	let decls = ArrayU.build_until_none (fun () -> ParseDecl.try_parse_decl l) in
	Ast.Modul(decls)
