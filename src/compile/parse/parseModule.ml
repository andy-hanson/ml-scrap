let parse_module_decls(p: Parser.t): Ast.decl array =
	ArrayU.build_array (fun () -> ParseDecl.try_parse_decl p)

let parse_module(p: Parser.t): Ast.modul =
	let start = Parser.pos p in
	let decls = parse_module_decls p in
	Ast.Modul(Parser.loc_from p start, decls)
