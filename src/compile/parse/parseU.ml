let unexpected(start: Loc.pos)(l: Lexer.t)(token: Token.t): 'a =
	CompileErrorU.raise (Lexer.loc_from l start) (CompileError.Unexpected token)

let must_skip(l: Lexer.t)(expected: Token.t): unit =
	let start, actual = Lexer.pos_next l in
	if actual != expected then raise (unexpected start l expected)

let parse_name(l: Lexer.t): Symbol.t =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.Name name ->
		name
	| x ->
		raise (unexpected start l x)

let parse_type_name(l: Lexer.t): Symbol.t =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.TypeName name ->
		name
	| x ->
		raise (unexpected start l x)
