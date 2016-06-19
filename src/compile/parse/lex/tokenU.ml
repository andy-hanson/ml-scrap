open Token

let keyword_to_string(keyword: t): string =
	match keyword with
	| Case -> "case"
	| Colon -> ":"
	| Fn -> "fn"
	| Equals -> "="
	| Ifc -> "ifc"
	| Rc -> "rc"
	| Un -> "un"
	| Ft -> "ft"
	| Indent -> "indent"
	| Dedent -> "dedent"
	| Newline -> "newline"
	| Lparen -> "("
	| Rparen -> ")"
	| End -> "EOF"
	| Name _ | TypeName _ | Operator _  | Literal _ -> assert false

let keyword =
	let name_to_keyword = Sym.Lookup.build_from_values Token.all_keywords (U.compose keyword_to_string Sym.of_string) in
	Sym.Lookup.try_get name_to_keyword

let output(out: 'o OutputU.t)(token: t) =
	match token with
	| Name s | TypeName s ->
		OutputU.out out "'%a'" Sym.output s
	| Literal value ->
		ValU.output out value
	| k ->
		OutputU.str out (keyword_to_string k)
