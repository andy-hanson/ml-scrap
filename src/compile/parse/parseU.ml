let unexpected(start: Loc.pos)(l: Lexer.t)(token: Token.t): 'a =
	CompileErrorU.raise (Lexer.loc_from l start) @@ CompileError.Unexpected token

let expect(start: Loc.pos)(l: Lexer.t)(expected: Token.t)(actual: Token.t): unit =
	if (expected != actual) then
		unexpected start l actual

let must_skip(l: Lexer.t)(expected: Token.t): unit =
	let start, actual = Lexer.pos_next l in
	expect start l expected actual

let parse_name(l: Lexer.t): Sym.t =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.Name name ->
		name
	| x ->
		raise @@ unexpected start l x

let parse_type_name(l: Lexer.t): Sym.t =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.TypeName name ->
		name
	| x ->
		raise @@ unexpected start l x
