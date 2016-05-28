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

let output(out: 'a BatIO.output)(token: t) =
	match token with
	| Name s ->
		OutputU.out out "'%s'" s.Symbol.name
	| TypeName s ->
		OutputU.out out "'%s'" s.Symbol.name
	| Literal value ->
		begin match value with
		| Val.Int i ->
			OutputU.out out "%d" i
		| Val.Float f ->
			OutputU.out out "%f" f
		| _ -> failwith "not a literal"
		end
	| k ->
		BatIO.nwrite out (keyword_to_string k)
