open Token

let keyword_to_string(keyword: t): string =
	match keyword with
	| At -> "@"
	| AtAt -> "@@"
	| Equals -> "="
	| Import -> "import"
	| Cs -> "cs"
	| Ck -> "ck"
	| Fn -> "fn"
	| Rt -> "rt"
	| Un -> "un"
	| Ft -> "ft"
	| Sn -> "sn"
	| _ -> assert false

let keyword: Sym.t -> t option =
	Sym.Lookup.create_partial_function_from_values Token.all_keywords @@ fun kw ->
		Sym.of_string @@ keyword_to_string kw

let output(out: 'o OutputU.t)(token: t) =
	let o fmt = OutputU.out out fmt in
	let s = OutputU.str out in
	match token with
	| Name s | TyName s | Operator s ->
		OutputU.out out "'%a'" Sym.output s
	| QuoteStart s ->
		o "QuoteStart(\"%s\")" @@ String.escaped s
	| Literal value ->
		AstOut.output_literal out value
	| Colon -> s ":"
	| DotDot -> s ".."
	| Indent -> s "indent"
	| Dedent -> s "dedent"
	| Newline -> s "newline"
	| Lparen -> s "("
	| Rparen -> s ")"
	| Lbracket -> s "["
	| Rbracket -> s "]"
	| RCurly -> s "}"
	| EOF -> s "EOF"
	| keyword ->
		s @@ keyword_to_string keyword
