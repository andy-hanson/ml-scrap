let unexpected(l: Lexer.t)(token: Token.t): 'a =
	CompileError.raise (Lexer.loc_at l) (CompileError.Unexpected token)

let must_skip(l: Lexer.t)(expected: Token.t): unit =
	let actual = Lexer.next l in
	if actual != expected then raise (unexpected l expected)

let parse_name(l: Lexer.t): Symbol.t =
	match Lexer.next l with
	| Token.Name name ->
		name
	| x ->
		raise (unexpected l x)

let parse_type_name(l: Lexer.t): Symbol.t =
	match Lexer.next l with
	| Token.TypeName name ->
		name
	| x ->
		raise (unexpected l x)
