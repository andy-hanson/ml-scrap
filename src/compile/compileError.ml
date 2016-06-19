type message =
	(* lexer *)
	| LeadingSpace
	| NumberMustHaveDigitsAfterDecimalPoint
	| TooMuchIndent
	| TrailingSpace
	| UnrecognizedCharacter of char
	(* parser *)
	| BlockCantEndInDeclare
	| CaseMustBeInLineContext
	| EmptyExpression
	| EqualsInExpression
	| FnNeedsParts
	| OrNeedsParts
	| PrecedingEquals
	| Unexpected of Token.t
	(* bind *)
	| CantBind of Sym.t
	| CantUseTypeAsValue
	| NameAlreadyBound of Sym.t * Binding.t
	(* checkTypes *)
	| CanOnlyCaseUnion of Type.t
	| CasePartType of Type.t array * Type.t
	| CasesUnhandled of Type.t array
	| CombineTypes of Type.t * Type.t
	| NotCallable of Type.t
	| NotExpectedType of Type.t * Type.t (* expected * actual *)
	| NumArgs of int * int (* n_params * n_args *)

type warning = Warning of Loc.t * message
exception T of warning
