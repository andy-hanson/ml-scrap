open CompileError

let raise(loc: Loc.t)(m: message): 'a =
	raise (T (Warning(loc, m)))

let check(cond: bool)(loc: Loc.t)(message: message): unit =
	if not cond then
		raise loc message

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
	| CaseMustBeInLineContext ->
		o "`case` can't appear in a sub-expression"
	| EmptyExpression ->
		o "Empty expression"
	| Unexpected token ->
		o "Unexpected token %a"
			Token.output token

	| CantBind name ->
		o "Can't bind `%a`"
			Symbol.output name
	| CantUseTypeAsValue ->
		o "Attempted to use a type as a value"
	| NameAlreadyBound(name, binding) ->
		o "Attempt to redeclare %a, defined as %a"
			Symbol.output name
			BindingU.output binding

	| CanOnlyCaseUnion typ ->
		o "Expected a union type, got a %a"
			TypeU.output typ
	| CaseLength(types, n) ->
		o "Expected a case for each of %a, got only %d cases"
			(OutputU.out_array TypeU.output)
			types n
	| CasePartType(expected_type, actual_type) ->
		o "Case should handle %a, but handles %a instead"
			TypeU.output expected_type
			TypeU.output actual_type
	| CombineTypes(a, b) ->
		o "Can't combine types %a and %a because they are not exactly equal and we don't infer unions yet"
			TypeU.output a
			TypeU.output b
	| NotCallable typ ->
		o "Expected a function, got a %a"
			TypeU.output typ
	| NotExpectedType(expected, actual) ->
		o "Expected %a, got %a"
			TypeU.output expected
			TypeU.output actual
	| NumArgs(n_params, n_args) ->
		o "Function needs %d parameters, but is given %d"
			n_params
			n_args
