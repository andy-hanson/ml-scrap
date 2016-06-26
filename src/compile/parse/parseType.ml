let f(l: Lexer.t): Ast.typ =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.TypeName name ->
		Ast.TypeAccess(Lexer.loc_from l start, name)
	| x ->
		ParseU.unexpected start l x
