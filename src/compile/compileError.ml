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
	(* typeCheck *)
	| CanOnlyCaseUnion of N.ty
	| CasePartType of N.ty array * N.ty
	| CasesUnhandled of N.ty array
	| CombineTypes of N.ty * N.ty
	| NotAFunction of N.ty
	| NotAValue of Ast.access
	| NotExpectedType of N.ty * N.ty (* expected * actual *)
	| NumArgs of int * int (* n_params * n_args *)

type warning = Warning of Loc.t * message
exception T of warning
