open Val

(* If this is raised, the typechecker was wrong. *)
exception CastFail

let equal(a: Val.t)(b: Val.t): bool =
	a = b

let bool_of = function
	| Bool b -> b
	| _ -> raise CastFail
let int_of = function
	| Int i -> i
	| _ -> raise CastFail
let float_of = function
	| Float f -> f
	| _ -> raise CastFail

let type_of = function
	| Bool _ -> Type.Bool
	| Int _ -> Type.Int
	| Float _ -> Type.Float
	| Fn({closure_info; _}, _) -> Type.Ft(closure_info.ft)
	| BuiltinFn({typ; _}) -> Type.Ft typ
	| Rc(typ, _) -> Type.Rc typ
	| Void -> Type.Void

let fn_info_name({ft = {Type.fname; _}; _}: fn_info): Sym.t =
	fname
let fn_name({info}: fn): Sym.t =
	fn_info_name info
let builtin_fn_name({typ = {Type.fname; _}; _}: builtin_fn): Sym.t =
	fname
let fn_arity({info = {ft; _}; _}: fn): int =
	TypeU.ft_arity ft

let rec output(out: 'o OutputU.t)(value: t): unit =
	let o fmt = OutputU.out out fmt in
	match value with
	| Bool b ->
		o "%b" b
	| Int i ->
		o "%d" i
	| Float f ->
		o "%f" f
	| Fn(_, closure_values) ->
		o "SOME CLOSURE(%a)"
			(*TODO*)
			(OutputU.out_array output) closure_values
	| BuiltinFn f ->
		o "BuiltinFn(%a)" Sym.output (builtin_fn_name f)
	| Rc(record, properties) ->
		let out_property(out: 'o OutputU.t)(({Type.prop_name; _}, value): Type.property * t): unit =
			OutputU.out out "%a=%a"
				Sym.output prop_name
				output value in
		o "%a(%a)"
			Sym.output record.Type.rname
			(OutputU.out_array_elements out_property) (ArrayU.zip record.Type.properties properties)
	| Void ->
		o "void"

let rec output_fn(out: 'o OutputU.t)({info = {ft; _}}: fn): unit =
	OutputU.out out "fn(%a)" TypeU.output_ft ft
and output_bytecode(out: 'o OutputU.t)(c: bytecode): unit =
	let o fmt = OutputU.out out fmt in
	match c with
	| CallStatic f ->
		o "CallFn(%a)" Sym.output (fn_name f)
	| CallBuiltin f ->
		o "CallBuiltin(%a)" Sym.output (builtin_fn_name f)
	| CallLambda ->
		o "CallLambda"
	| Case parts ->
		o "Case(%a)" (OutputU.out_array (OutputU.out_pair TypeU.output OutputU.output_int)) parts
	| Const v ->
		o "Const(%a)" output v
	| Construct r ->
		o "Construct(%a)" Sym.output r.Type.rname
	| Drop ->
		o "Drop"
	| Load i ->
		o "Load(%d)" i
	| Goto i ->
		o "Goto(%d)" i
	| GotoIfFalse i ->
		o "GotoIfFalse(%d)" i
	| Return ->
		o "Return"
	| UnLet ->
		o "UnLet"
and output_code(out: 'o OutputU.t)({bytecodes; locs}: code): unit =
	OutputU.out out "Code(%a, %a)"
		(OutputU.out_array output_bytecode) bytecodes
		CodeLocs.output locs
