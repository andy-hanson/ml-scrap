type t =
	| Name of Sym.t
	| TypeName of Sym.t
	| Operator of Sym.t
	| Literal of N.primitive
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
	| Cn
	| Rt
	| Un
	| Ft
	| Ct
	| Sn
	(* Grouping *)
	| Indent
	| Dedent
	| Newline
	| Lparen
	| Rparen
	| RCurly
	| QuoteStart of string
	| EOF

(* Just the keywords with text names *)
let all_keywords: t array =
	[| At; AtAt; Equals; Cs; Ck; Import; Rt; Un; Ft; Ct; Fn; Cn |]
