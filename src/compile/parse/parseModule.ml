let parse_module_decls(l: Lexer.t): Ast.decl array =
	ArrayU.build_array (fun () -> ParseDecl.try_parse_decl l)

let parse_module(l: Lexer.t): Ast.modul =
	let start = Lexer.pos l in
	let decls = parse_module_decls l in
	Ast.Modul(Lexer.loc_from l start, decls)
