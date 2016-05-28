type message =
	(* lexer *)
	| LeadingSpace
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

type warning = Warning of Loc.t * message
exception T of warning

let raise(loc: Loc.t)(m: message): 'a =
	raise (T (Warning(loc, m)))

let output_message(out: 'a BatIO.output)(m: message): unit =
	let str = BatIO.nwrite out in
	match m with
	| LeadingSpace ->
		str "Line may not begin with a space"
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
		OutputU.out out "Can't bind %a" Symbol.output name
	| CantUseTypeAsValue ->
		str "Attempted to use a type as a value"
	| NameAlreadyBound name ->
		OutputU.out out "Attempt to redeclare %a" Symbol.output name

let check(cond: bool)(loc: Loc.t)(message: message): unit =
	if not cond then
		raise loc message
