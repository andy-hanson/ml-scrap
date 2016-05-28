let f(p: Parser.t): Ast.typ =
	let start = Parser.pos p in
	let type_name = ParseU.parse_type_name p in
	Ast.TypeAccess(Parser.loc_from p start, type_name)
