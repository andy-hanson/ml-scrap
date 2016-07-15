let f_with_start(l: Lexer.t)(start: Loc.pos)(next: Token.t): Ast.ty =
	match next with
	| Token.TypeName name ->
		Ast.TypeAccess(Lexer.loc_from l start, name)
	| x ->
		ParseU.unexpected start l x

let f(l: Lexer.t): Ast.ty =
	let start, next = Lexer.pos_next l in
	f_with_start l start next
