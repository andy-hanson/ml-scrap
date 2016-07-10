type message =
	(* lexer *)
	| LeadingSpace
	| NumberMustHaveDigitsAfterDecimalPoint
	| TooMuchIndent
	| TrailingSpace
	| UnrecognizedCharacter of char
	(* parser *)
	| BlockCantEndInDeclare
	| CsMustBeInLineContext
	| EmptyExpression
	| EqualsInExpression
	| FnNeedsParts
	| OrNeedsParts
	| PrecedingEquals
	| Unexpected of Token.t
	(* bind *)
	| CantBind of Sym.t
	| CantUseTypeAsValue
	| NameAlreadyBound of Sym.t * Binding.v
	| TypeNameAlreadyBound of Sym.t * Binding.ty
	(* typeCheck *)
	| CanOnlyCsUnion of N.ty
	| CsPartType of N.ty array * N.ty
	| CasesUnhandled of N.ty array
	| CombineTypes of N.ty * N.ty
	| NotAFunction of N.ty
	| NotAValue of Ast.access
	| NotExpectedType of N.ty * N.ty (* expected * actual *)
	| NumArgs of int * int (* n_params * n_args *)

type t = Loc.t * message
exception Exn of t
