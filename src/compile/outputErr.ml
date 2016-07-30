open Err

let name: message -> string = function
	| CircularDependency _ -> "CircularDependency"
	| CantFindLocalModule _ -> "CantFindLocalModule"

	(* lexer *)
	| LeadingSpace -> "LeadingSpace"
	| NumberMustHaveDigitsAfterDecimalPoint -> "NumberMustHaveDigitsAfterDecimalPoint"
	| TooMuchIndent -> "TooMuchIndent"
	| TrailingSpace -> "TrailingSpace"
	| UnrecognizedCharacter _ -> "UnrecognizedCharacter"

	(* parser *)
	| BlockCantEndInDeclare -> "BlockCantEndInDeclare"
	| CsMustBeInLineContext -> "CsMustBeInLineContext"
	| EmptyExpression -> "EmptyExpression"
	| EqualsInExpression -> "EqualsInExpression"
	| FnNeedsParts -> "FnNeedsParts"
	| OrNeedsParts -> "OrNeedsParts"
	| PrecedingEquals -> "PrecedingEquals"
	| Unexpected _ -> "Unexpected"

	(* bind *)
	| CantBind _ -> "CantBind"
	| CantUseTypeAsValue -> "CantUseTypeAsValue"
	| ModuleHasNoMember(_, _) -> "ModuleHasNoMember"
	| NameAlreadyBound(_, _) -> "NameAlreadyBound"
	| TypeNameAlreadyBound(_, _) -> "TypeNameAlreadyBound"

	(* typeCheck *)
	| CanOnlyCsUnion _ -> "CanOnlyCsUnion"
	| CantConvertRtMissingProperty(_, _, _) -> "CantConvertRtMissingProperty"
	| CsPartType(_, _) -> "CsPartType"
	| CasesUnhandled _ -> "CasesUnhandled"
	| CombineTypes(_, _) -> "CombineTypes"
	| GenInstParameters(_, _) -> "GenInstParameters"
	| NotAFunction _ -> "NotAFunction"
	| NotAValue _ -> "NotAValue"
	| NotARc _ -> "NotARc"
	| NoSuchProperty(_, _) -> "NoSuchProperty"
	| NotExpectedType(_, _) -> "NotExpectedType"
	| NotExpectedTypeAndNoConversion(_, _) -> "NotExpectedTypeAndNoConversion"
	| NumArgs(_, _) -> "NumArgs"

let output_message(out: 'o OutputU.t)(m: message): unit =
	let o fmt = OutputU.out out fmt in
	match m with
	| CircularDependency path ->
		o "There is a circular dependency involving %a"
			Path.output path
	| CantFindLocalModule(rel_path, regular_path, main_path) ->
		o "Can't find any module %a. Tried %a and %a."
			Path.output_rel rel_path
			Path.output regular_path
			Path.output main_path

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
	| ModuleHasNoMember({N.Compiler.path; _}, name) ->
		o "Module %a has no member %a"
			Path.output path
			Sym.output name
	| NameAlreadyBound(name, binding) ->
		o "Attempt to redeclare %a; already defined at %a"
			Sym.output name
			BindingU.output_v binding
	| TypeNameAlreadyBound(name, binding) ->
		o "Attempt to redeclare %a; already defined at %a"
			Sym.output name
			BindingU.output_ty binding

	| CanOnlyCsUnion ty ->
		o "Expected a union type, got a %a"
			TyOut.output ty
	| CsPartType(possible_tys, handled_ty) ->
		o "`cs` should handle one of %a, but handles %a instead"
			(ArrayU.output TyOut.output) possible_tys
			TyOut.output handled_ty
	| CantConvertRtMissingProperty(convert_to, convert_from, prop_name) ->
		o "Can't convert to %a from %a: missing property %a"
			TyOut.output_rt convert_to
			TyOut.output_rt convert_from
			Sym.output prop_name
	| CasesUnhandled unhandled_tys ->
		o "The following cases are not handled: %a"
			(ArrayU.output TyOut.output) unhandled_tys
	| CombineTypes(a, b) ->
		o "Can't combine types %a and %a because they are not exactly equal and we don't infer unions yet"
			TyOut.output a
			TyOut.output b
	| GenInstParameters(expected, actual) ->
		o "Expected %i generic parameters, got %i" expected actual
	| NotAFunction ty ->
		o "Expected a function, got a %a"
			TyOut.output ty
	| NotAValue(_, name) ->
		(*TODO: what's the first arg for then?*)
		o "Not a value: %a"
			Sym.output name
	| NotARc(ty) ->
		o "Expected a record, got: %a."
			TyOut.output ty
	| NoSuchProperty(rt, name) ->
		o "Type %a has no property %a"
			TyOut.output_rt rt
			Sym.output name
	| NotExpectedType(expected, actual) ->
		o "Expected a value of type %a, got a %a"
			TyOut.output expected
			TyOut.output actual
	| NotExpectedTypeAndNoConversion(expected, actual) ->
		o "Expected to convert to type %a, but got a %a which can't convert"
			TyOut.output expected
			TyOut.output actual
	| NumArgs(n_params, n_args) ->
		o "Function needs %d parameters, but is given %d"
			n_params
			n_args

let output(translate_loc: Path.t -> Loc.t -> Loc.lc_loc)(out: 'o OutputU.t)({Err.path; loc; message}: Err.t): unit =
	let lc_loc = translate_loc path loc in
	OutputU.out out "%s at %a %a:\n%a\n\n"
		(name message)
		Path.output path
		Loc.output_lc_loc lc_loc
		output_message message
