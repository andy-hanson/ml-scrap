let unexpected(start: Loc.pos)(l: Lexer.t)(token: Token.t): 'a =
	CompileError.raise (Lexer.loc_from l start) (CompileError.Unexpected token)

let must_skip(l: Lexer.t)(expected: Token.t): unit =
	let start = Lexer.pos l in
	let actual = Lexer.next l in
	if actual != expected then raise (unexpected start l expected)

let parse_name(l: Lexer.t): Symbol.t =
	let start = Lexer.pos l in
	match Lexer.next l with
	| Token.Name name ->
		name
	| x ->
		raise (unexpected start l x)

let parse_type_name(l: Lexer.t): Symbol.t =
	let start = Lexer.pos l in
	match Lexer.next l with
	| Token.TypeName name ->
		name
	| x ->
		raise (unexpected start l x)
