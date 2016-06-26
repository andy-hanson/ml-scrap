type t =
	| Name of Sym.t
	| TypeName of Sym.t
	| Operator of Sym.t
	| Literal of N.v
	(* Keywords *)
	| Equals
	(* Expression keywords *)
 	| Case
	| Colon
	(* Declaration keywords *)
	| Fn
	| Cn
	| Rt
	| Un (*TODO:UT*)
	| Ft
	| Ct
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
	[| Equals; Case; Fn; Cn; Rt; Un; Ft; Ct |]
