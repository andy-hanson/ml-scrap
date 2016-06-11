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
	| EmptyExpression
	| Unexpected of Token.t
	(* bind *)
	| CantBind of Symbol.t
	| CantUseTypeAsValue
	| NameAlreadyBound of Symbol.t * Binding.t
	(* checkTypes *)
	| NotCallable of Type.t
	| NotExpectedType of Type.t * Type.t (* expected * actual *)
	| NumArgs of int * int (* n_params * n_args *)

type warning = Warning of Loc.t * message
exception T of warning

let raise(loc: Loc.t)(m: message): 'a =
	raise (T (Warning(loc, m)))

let output_message(out: 'o OutputU.t)(m: message): unit =
	let o fmt = OutputU.out out fmt in
	match m with
	| LeadingSpace ->
		o "Line may not begin with a space"
	| NegMustPrecedeNumber ->
		o "`-` must be followed by a space or a number"
	| NumberMustHaveDigitsAfterDecimalPoint ->
		o "Number must have digits after decimal point"
	| TooMuchIndent ->
		o "Too much indent!"
	| TrailingSpace ->
		o "Trailing space"
	| UnrecognizedCharacter ch ->
		o "Unrecognized character `%c`" ch

	| BlockCantEndInDeclare ->
		o "Last line of block can't be a declaration"
	| EmptyExpression ->
		o "Empty expression"
	| Unexpected token ->
		o "Unexpected token %a" Token.output token

	| CantBind name ->
		o "Can't bind `%a`" Symbol.output name
	| CantUseTypeAsValue ->
		o "Attempted to use a type as a value"
	| NameAlreadyBound(name, binding) ->
		o "Attempt to redeclare %a, defined as %a" Symbol.output name Binding.output binding

	| NotCallable typ ->
		o "Expected a function, got a %a" Type.output typ
	| NotExpectedType(expected, actual) ->
		o "Expected %a, got %a" Type.output expected Type.output actual
	| NumArgs(n_params, n_args) ->
		o "Function needs %d parameters, but is given %d" n_params n_args

let check(cond: bool)(loc: Loc.t)(message: message): unit =
	if not cond then
		raise loc message
