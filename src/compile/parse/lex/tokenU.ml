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
	| Cn -> "cn"
	| Rt -> "rt"
	| Un -> "un"
	| Ft -> "ft"
	| Ct -> "ct"
	| Sn -> "sn"
	| _ -> assert false

let keyword =
	let name_to_keyword = Sym.Lookup.build_from_values Token.all_keywords (U.compose keyword_to_string Sym.of_string) in
	Sym.Lookup.try_get name_to_keyword

let output(out: 'o OutputU.t)(token: t) =
	let o fmt = OutputU.out out fmt in
	let s = OutputU.str out in
	match token with
	| Name s | TypeName s | Operator s ->
		OutputU.out out "'%a'" Sym.output s
	| QuoteStart s ->
		o "QuoteStart(\"%s\")" @@ String.escaped s
	| Literal value ->
		ValU.output_primitive out value
	| Colon -> s ":"
	| DotDot -> s ".."
	| Indent -> s "indent"
	| Dedent -> s "dedent"
	| Newline -> s "newline"
	| Lparen -> s "("
	| Rparen -> s ")"
	| RCurly -> s "}"
	| EOF -> s "EOF"
	| keyword ->
		s @@ keyword_to_string keyword
