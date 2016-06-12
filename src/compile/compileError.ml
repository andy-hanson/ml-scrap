type message =
	(* lexer *)
	| LeadingSpace
	| NegMustPrecedeNumber
	| NumberMustHaveDigitsAfterDecimalPoint
	| TooMuchIndent
	| TrailingSpace
	| UnrecognizedCharacter of char
	(* parser *)
	| BlockCantEndInDeclare
	| CaseMustBeInLineContext
	| EmptyExpression
	| Unexpected of Token.t
	(* bind *)
	| CantBind of Symbol.t
	| CantUseTypeAsValue
	| NameAlreadyBound of Symbol.t * Binding.t
	(* checkTypes *)
	| CanOnlyCaseUnion of Type.t
	| CaseLength of Type.t array * int
	| CasePartType of Type.t * Type.t
	| CombineTypes of Type.t * Type.t
	| NotCallable of Type.t
	| NotExpectedType of Type.t * Type.t (* expected * actual *)
	| NumArgs of int * int (* n_params * n_args *)

type warning = Warning of Loc.t * message
exception T of warning
