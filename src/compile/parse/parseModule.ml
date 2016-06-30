let parse_module(l: Lexer.t): Ast.modul =
	ArrayU.build_until_none @@ fun () -> ParseDecl.try_parse_decl l
