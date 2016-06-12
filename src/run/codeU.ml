open Code

let fn_arity({params; _}: fn): int =
	Array.length params

let output_parameter(out: 'o OutputU.t)({param_name; param_type}: parameter): unit =
	OutputU.out out "{ param_name = %a; param_type = %a }" Symbol.output param_name TypeU.output param_type
let rec output_fn(out: 'o OutputU.t)({fname; return_type; params; code}: fn): unit =
	OutputU.out out "{ fname: %a, return_type: %a; params: %a, code: %a }"
		Symbol.output fname
		TypeU.output return_type
		(OutputU.out_array output_parameter)
		params output code
and output_bytecode(out: 'o OutputU.t)(c: bytecode): unit =
	let str = OutputU.str out in
	match c with
	| Call f ->
		OutputU.out out "CallFn(%a)" Symbol.output f.fname
	| Case parts ->
		OutputU.out out "Case(%a)" (OutputU.out_array (OutputU.out_pair TypeU.output OutputU.output_int)) parts
	| CallBuiltin b ->
		OutputU.out out "CallBuiltin(%a)" BuiltinU.output b
	| Const v ->
		OutputU.out out "Const(%a)" ValU.output v
	| Construct r ->
		OutputU.out out "Construct(%a)" Symbol.output r.Type.rname
	| Drop ->
		str "Drop"
	| Load i ->
		OutputU.out out "Load(%d)" i
	| Goto i ->
		OutputU.out out "Goto(%d)" i
	| GotoIfFalse i ->
		OutputU.out out "GotoIfFalse(%d)" i
	| Return ->
		str "Return"
	| UnLet ->
		str "UnLet"
and output(out: 'o OutputU.t)(code: t): unit =
	OutputU.out out "Code(%a)" (OutputU.out_array output_bytecode) code
