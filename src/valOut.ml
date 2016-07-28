open N.V
open N.Ty
open N.Code

let output_primitive(out: 'o OutputU.t)(p: primitive): unit =
	let o fmt = OutputU.out out fmt in
	begin match p with
	| Bool b ->
		o "%b" b
	| Int i ->
		o "%d" i
	| Float f ->
		o "%f" f
	| String s ->
		o "\"%s\"" @@ String.escaped s
	| Void ->
		o "void"
	end

let rec output_fn(out: 'o OutputU.t)(fn: fn): unit =
	let o fmt = OutputU.out out fmt in
	match fn with
	| DeclaredFn fn ->
		o "DeclaredFn(%a)" Sym.output (ValU.fn_name fn)
	| BuiltinFn b ->
		o "BuiltinFn(%a)" Sym.output @@ ValU.builtin_fn_name b
	| PartialFn {partially_applied; partial_args} ->
		OutputU.out out "PartialFn(%a, %a)"
			output_fn partially_applied
			(ArrayU.output output) partial_args
	| Ctr {rt_origin; _} ->
		OutputU.out out "Ctr(%a)"
			TyOut.output_rt_origin rt_origin

(*TODO: NOut module*)
and output(out: 'o OutputU.t)(value: v): unit =
	let o fmt = OutputU.out out fmt in
	match value with
	| Primitive p ->
		output_primitive out p
	| Fn f ->
		output_fn out f
	| Rc({rt_id = _; rt_origin; properties}, property_values) ->
		let out_property(out: 'o OutputU.t)(((name, _), value): property * v): unit =
			OutputU.out out "%a=%a"
				Sym.output name
				output value in
		o "%a(%a)"
			TyOut.output_rt_origin rt_origin
			(ArrayU.output_elements out_property) (ArrayU.zip properties property_values)

and output_declared_fn(out: 'o OutputU.t)({fn_ty; _}: declared_fn): unit =
	OutputU.out out "Fn(%a)"
		TyOut.output_ft_or_gen fn_ty

and output_pattern(out: 'o OutputU.t)(pattern: pattern): unit =
	match pattern with
	| PSingle ->
		OutputU.str out "PSingle"
	| PDestruct patterns ->
		ArrayU.output output_pattern out patterns

and output_bytecode(out: 'o OutputU.t)(c: bytecode): unit =
	let o fmt = OutputU.out out fmt in
	match c with
	| Call ->
		o "Call"
	| Cs parts ->
		let output_part = OutputU.out_pair TyOut.output_brief OutputU.output_int in
		o "Cs(%a)" (ArrayU.output output_part) parts
	| Const v ->
		o "Const(%a)" output v
	| CnvRc(rt, indices) ->
		o "CnvRc(%a, %a)"
			TyOut.output_rt rt
			(ArrayU.output OutputU.output_int) indices
	| Destruct pattern ->
		o "Destruct(%a)" (ArrayU.output output_pattern) pattern
	| Drop ->
		o "Drop"
	| Dup ->
		o "Dup"
	| Load i ->
		o "Load(%d)" i
	| GetProperty i ->
		o "GetProperty(%i)" i
	| Goto i ->
		o "Goto(%d)" i
	| GotoIfFalse i ->
		o "GotoIfFalse(%d)" i
	| Return ->
		o "Return"
	| UnLet n ->
		o "UnLet(%i)" n
	| Partial arity ->
		o "Partial(%i)" arity
	| Quote strings ->
		o "Quote(%a)"
			(ArrayU.output OutputU.output_string_escaped) strings
	| Check ->
		o "Check"
	| Nil ->
		o "Nil"
and output_code(out: 'o OutputU.t)({bytecodes; locs}: code): unit =
	OutputU.out out "Code(%a, %a)"
		(ArrayU.output output_bytecode) bytecodes
		CodeLocs.output locs

let to_string(v: v): string =
	OutputU.out_to_string "%a" output v
