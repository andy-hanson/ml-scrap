open N.Ty

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
	| ModuleHasNoMember of N.Compiler.modul * Sym.t
	| NameAlreadyBound of Sym.t * Binding.v
	| TypeNameAlreadyBound of Sym.t * Binding.ty

	(* typeCheck *)
	| CanOnlyCsUnion of ty
	| CantConvertRtMissingProperty of rt * rt * Sym.t
	| CsPartType of ty array * ty
	| CasesUnhandled of ty array
	| CombineTypes of ty * ty
	| GenInstParameters of int * int
	| NotAFunction of ty
	| NotAValue of Ast.access
	| NotARc of ty
	| NoSuchProperty of rt * Sym.t
	| NotExpectedType of ty * ty (* expected * actual *)
	| NumArgs of int * int (* n_params * n_args *)

type t = Loc.t * message
exception Exn of t
