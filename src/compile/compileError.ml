type message =
	(* lexer *)
	| LeadingSpace
	| NegMustPrecedeNumber
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
	| NameAlreadyBound of Symbol.t
	(* checkTypes *)
	| NotCallable (*TODO:args*)
	| NotExpectedType of Type.t * Type.t (* expected * actual *)
	| NumArgs of int * int (* n_params * n_args *)

type warning = Warning of Loc.t * message
exception T of warning

let raise(loc: Loc.t)(m: message): 'a =
	raise (T (Warning(loc, m)))

let output_message(out: 'a OutputU.t)(m: message): unit =
	let str = OutputU.str out in
	match m with
	| LeadingSpace ->
		str "Line may not begin with a space"
	| NegMustPrecedeNumber ->
		str "`-` must be followed by a space or a number."
	| TooMuchIndent ->
		str "Too much indent!"
	| TrailingSpace ->
		str "Trailing space"
	| UnrecognizedCharacter ch ->
		OutputU.out out "Unrecognized character `%c`" ch

	| BlockCantEndInDeclare ->
		str "Last line of block can't be a declaration"
	| EmptyExpression ->
		str "Empty expression"
	| Unexpected token ->
		OutputU.out out "Unexpected token %a" Token.output token

	| CantBind name ->
		OutputU.out out "Can't bind `%a`" Symbol.output name
	| CantUseTypeAsValue ->
		str "Attempted to use a type as a value"
	| NameAlreadyBound name ->
		OutputU.out out "Attempt to redeclare %a" Symbol.output name

	| NotCallable ->
		str "This is not a callable type"
	| NotExpectedType(expected, actual) ->
		OutputU.out out "Expected %a, got %a" Type.output expected Type.output actual
	| NumArgs(n_params, n_args) ->
		OutputU.out out "Function needs %d parameters, but is given %d" n_params n_args

let check(cond: bool)(loc: Loc.t)(message: message): unit =
	if not cond then
		raise loc message
