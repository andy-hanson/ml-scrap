type t =
	| Name of Sym.t
	| TyName of Sym.t
	| Operator of Sym.t
	| Literal of Ast.literal_value
	(* Keywords *)
	| At
	| AtAt
	| Equals
	| Dot
	| DotDot
	| Import
	(* Expression keywords *)
	| Cs
	| Colon
	| Ck
	(* Declaration keywords *)
	| Fn
	| Rt
	| Un
	| Ft
	| Sn
	(* Grouping *)
	| Indent
	| Dedent
	| Newline
	| Lparen
	| Rparen
	| Lbracket
	| Rbracket
	| RCurly
	| QuoteStart of string
	| EOF

(* Just the keywords with text names *)
let all_keywords: t array =
	[| At; AtAt; Equals; Cs; Ck; Import; Rt; Un; Ft; Fn |]
