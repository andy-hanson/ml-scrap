type t =
	| Name of Symbol.t
	| TypeName of Symbol.t
	| Literal of Val.t
	(* Keywords *)
 	| Case
	| Colon
	| Comma
	| Equals
	| Fn
	| Ifc
	| Rec
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
	[| Case; Fn; Ifc; Rec |]

let keyword_to_string(keyword: t): string =
	match keyword with
	| Case -> "case"
	| Colon -> ":"
	| Comma -> ","
	| Equals -> "="
	| Fn -> "fn"
	| Ifc -> "ifc"
	| Rec -> "rec"
	| Indent -> "indent"
	| Dedent -> "dedent"
	| Newline -> "newline"
	| Lparen -> "("
	| Rparen -> ")"
	(* | Start -> "START" *)
	| End -> "EOF"
	| _ -> failwith "Not a keyword!"

(* boilerplate *)

let output(out: 'a OutputU.t)(token: t) =
	match token with
	| Name s | TypeName s ->
		OutputU.out out "'%a'" Symbol.output s
	| Literal value ->
		Val.output out value
	| k ->
		OutputU.str out (keyword_to_string k)
