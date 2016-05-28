let f(p: Parser.t): Ast.local_declare =
	let start = Parser.pos p in
	let name = ParseU.parse_name p in
	let typ = ParseType.f p in
	Ast.LocalDeclare(Parser.loc_from p start, name, typ)
