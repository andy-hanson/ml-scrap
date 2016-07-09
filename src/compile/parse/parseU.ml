let unexpected(start: Loc.pos)(l: Lexer.t)(token: Token.t): 'a =
	ErrU.raise (Lexer.loc_from l start) @@ Err.Unexpected token

let expect(start: Loc.pos)(l: Lexer.t)(expected: Token.t)(actual: Token.t): unit =
	if (expected != actual) then
		unexpected start l actual

let must_skip(l: Lexer.t)(expected: Token.t): unit =
	let start, actual = Lexer.pos_next l in
	expect start l expected actual

let parse_name_with_loc(l: Lexer.t): Loc.t * Sym.t =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.Name name ->
		Lexer.loc_from l start, name
	| x ->
		unexpected start l x

let parse_name(l: Lexer.t): Sym.t =
	snd @@ parse_name_with_loc l

let parse_type_name(l: Lexer.t): Sym.t =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.TypeName name ->
		name
	| x ->
		unexpected start l x
