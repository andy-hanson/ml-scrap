let f(l: Lexer.t): Ast.typ =
	let start = Lexer.pos l in
	let type_name = ParseU.parse_type_name l in
	Ast.TypeAccess(Lexer.loc_from l start, type_name)
