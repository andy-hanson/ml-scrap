open Err

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
	| CsMustBeInLineContext ->
		o "`cs` can't appear in a sub-expression"
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

	| CanOnlyCsUnion typ ->
		o "Expected a union type, got a %a"
			TyU.output typ
	| CsPartType(possible_types, handled_type) ->
		o "`cs` should handle one of %a, but handles %a instead"
			(OutputU.out_array TyU.output) possible_types
			TyU.output handled_type
	| CasesUnhandled unhandled_types ->
		o "The following cases are not handled: %a"
			(OutputU.out_array TyU.output) unhandled_types
	| CombineTypes(a, b) ->
		o "Can't combine types %a and %a because they are not exactly equal and we don't infer unions yet"
			TyU.output a
			TyU.output b
	| NotAFunction typ ->
		o "Expected a function, got a %a"
			TyU.output typ
	| NotAValue(_, name) ->
		o "Not a vaule: %a"
			Sym.output name
	| NotExpectedType(expected, actual) ->
		o "Expected %a, got %a"
			TyU.output expected
			TyU.output actual
	| NumArgs(n_params, n_args) ->
		o "Function needs %d parameters, but is given %d"
			n_params
			n_args
