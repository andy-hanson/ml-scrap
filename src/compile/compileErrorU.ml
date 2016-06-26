open CompileError

let raise(loc: Loc.t)(m: message): 'a =
	raise @@ T(Warning(loc, m))

let check(cond: bool)(loc: Loc.t)(message: message): unit =
	if not cond then
		raise loc message

let output_message(out: 'o OutputU.t)(m: message): unit =
	let o fmt = OutputU.out out fmt in
	match m with
	| LeadingSpace ->
		o "Line may not begin with a space"
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
	| EqualsInExpression ->
		o "`=` can not appear inside an expression"
	| FnNeedsParts ->
		o "`Fn` type needs at least 2 arguments"
	| OrNeedsParts ->
		o "`Or` type needs at least 2 arguments"
	| PrecedingEquals ->
		o "`=` must be preceded by a name"
	| Unexpected token ->
		o "Unexpected token %a"
			TokenU.output token

	| CantBind name ->
		o "Can't bind `%a`"
			Sym.output name
	| CantUseTypeAsValue ->
		o "Attempted to use a type as a value"
	| NameAlreadyBound(name, binding) ->
		o "Attempt to redeclare %a, defined as %a"
			Sym.output name
			BindingU.output binding

	| CanOnlyCaseUnion typ ->
		o "Expected a union type, got a %a"
			TypeU.output typ
	| CasePartType(possible_types, handled_type) ->
		o "Case should handle one of %a, but handles %a instead"
			(OutputU.out_array TypeU.output) possible_types
			TypeU.output handled_type
	| CasesUnhandled unhandled_types ->
		o "The following cases are not handled: %a"
			(OutputU.out_array TypeU.output) unhandled_types
	| CombineTypes(a, b) ->
		o "Can't combine types %a and %a because they are not exactly equal and we don't infer unions yet"
			TypeU.output a
			TypeU.output b
	| NotAFunction typ ->
		o "Expected a function, got a %a"
			TypeU.output typ
	| NotAValue(_, name) ->
		o "Not a vaule: %a"
			Sym.output name
	| NotExpectedType(expected, actual) ->
		o "Expected %a, got %a"
			TypeU.output expected
			TypeU.output actual
	| NumArgs(n_params, n_args) ->
		o "Function needs %d parameters, but is given %d"
			n_params
			n_args
