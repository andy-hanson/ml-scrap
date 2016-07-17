type message =
	(* module loader *)
	| CircularDependency of Path.t
	| CantFindLocalModule of Path.rel * Path.t * Path.t

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
	| ModuleHasNoMember of N.modul * Sym.t
	| NameAlreadyBound of Sym.t * Binding.v
	| TypeNameAlreadyBound of Sym.t * Binding.ty

	(* typeCheck *)
	| CanOnlyCsUnion of N.ty
	| CantConvertRtMissingProperty of N.rt * N.rt * Sym.t
	| CsPartType of N.ty array * N.ty
	| CasesUnhandled of N.ty array
	| CombineTypes of N.ty * N.ty
	| NotAFunction of N.ty
	| NotAValue of Ast.access
	| NotARc of N.ty
	| NoSuchProperty of N.rt * Sym.t
	| NotExpectedType of N.ty * N.ty (* expected * actual *)
	| NumArgs of int * int (* n_params * n_args *)

type t = Loc.t * message
exception Exn of t
