type t =
	| Name of Symbol.t
	| TypeName of Symbol.t
	| Literal of Val.t
	(* Keywords *)
	(* Expression keywords *)
 	| Case
	| Colon
	(* Declaration keywords *)
	| Fn
	| Ifc
	| Rc
	(* Type keywords *)
	| Or
	(* Grouping *)
	| Indent
	| Dedent
	| Newline
	| Lparen
	| Rparen
	(* Always emitted at start of lexing *)
	(* | Start *)
	| End

(* Just the keywords with text names *)
let all_keywords: t array =
	[| Case; Fn; Ifc; Or; Rc |]

let keyword_to_string(keyword: t): string =
	match keyword with
	| Case -> "case"
	| Colon -> ":"
	| Fn -> "fn"
	| Ifc -> "ifc"
	| Rc -> "rc"
	| Or -> "Or"
	| Indent -> "indent"
	| Dedent -> "dedent"
	| Newline -> "newline"
	| Lparen -> "("
	| Rparen -> ")"
	(* | Start -> "START" *)
	| End -> "EOF"
	| _ -> failwith "Not a keyword!"

let output(out: 'o OutputU.t)(token: t) =
	match token with
	| Name s | TypeName s ->
		OutputU.out out "'%a'" Symbol.output s
	| Literal value ->
		ValU.output out value
	| k ->
		OutputU.str out (keyword_to_string k)
