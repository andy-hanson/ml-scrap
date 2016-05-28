let unexpected(p: Parser.t)(token: Token.t): 'a =
	CompileError.raise (Parser.loc_at p) (CompileError.Unexpected token)

let must_skip(p: Parser.t)(expected: Token.t): unit =
	let actual = Parser.next p in
	if actual != expected then raise (unexpected p expected)

let parse_name(p: Parser.t): Symbol.t =
	match Parser.next p with
	| Token.Name sym -> sym
	| x -> raise (unexpected p x)

let parse_type_name(p: Parser.t): Symbol.t =
	let name = Parser.next p in
	match name with
	| Token.TypeName sym -> sym
	| _ -> raise (unexpected p name)
