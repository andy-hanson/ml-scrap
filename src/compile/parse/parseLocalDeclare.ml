(*TODO: this doesn't need to be its own file*)
let f(l: Lexer.t): Ast.local_declare =
	let start = Lexer.pos l in
	let name = ParseU.parse_name l in
	let typ = ParseType.f l in
	Ast.LocalDeclare(Lexer.loc_from l start, name, typ)
