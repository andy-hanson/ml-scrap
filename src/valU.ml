open N

(* If this is raised, the typechecker was wrong. *)
exception CastFail

let equal(a: v)(b: v): bool =
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
	| Bool _ -> TBool
	| Int _ -> TInt
	| Float _ -> TFloat
	| Fn {fn_type; _} ->
		begin match fn_type with
		| FnFt ft -> Ft ft
		| FnCt ct -> Ct ct
		end
	| BuiltinFn {builtin_ft; _} -> Ft builtin_ft
	(*| Cn {ct; _} -> Ct ct*)
	| Rc(rt, _) -> Rt rt
	| Void -> TVoid
	| World -> BuiltinType.world

let fn_name({fn_type; _}: fn): Sym.t =
	match fn_type with
	| FnFt {fname; _} -> fname
	| FnCt {cname; _} -> cname
let builtin_fn_name({builtin_ft = {N.fname; _}; _}: builtin_fn): Sym.t =
	fname
let fn_arity({fn_type; _}: fn): int =
	match fn_type with
	| FnFt ft -> TypeU.ft_arity ft
	| FnCt _ -> 1
(*let cn_name({ct = {N.cname; _}; _}: cn): Sym.t =
	cname*)

(*TODO: NOut module*)
let rec output(out: 'o OutputU.t)(value: v): unit =
	let o fmt = OutputU.out out fmt in
	match value with
	| Bool b ->
		o "%b" b
	| Int i ->
		o "%d" i
	| Float f ->
		o "%f" f
	| Fn({fn_type; _}) ->
		o "Fn(%a)" TypeU.output_fn_type fn_type
	| BuiltinFn fn ->
		o "BuiltinFn(%a)" Sym.output (builtin_fn_name fn)
	(*TODO:KILL | Cn({cn_name; ct; _}) ->
		o "Cn(%a, %a)"
			Sym.output cn_name
			TypeU.output_ct ct*)
	| Rc({rname; properties}, property_values) ->
		let out_property(out: 'o OutputU.t)(((name, _), value): N.property * v): unit =
			OutputU.out out "%a=%a"
				Sym.output name
				output value in
		o "%a(%a)"
			Sym.output rname
			(OutputU.out_array_elements out_property) (ArrayU.zip properties property_values)
	| Void ->
		o "void"
	| World ->
		o "world"

let rec output_fn(out: 'o OutputU.t)({fn_type; _}: fn): unit =
	OutputU.out out "fn(%a)" TypeU.output_fn_type fn_type
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
		o "Case(%a)" (OutputU.out_array @@ OutputU.out_pair TypeU.output OutputU.output_int) parts
	| Const v ->
		o "Const(%a)" output v
	| Construct {rname; _} ->
		o "Construct(%a)" Sym.output rname
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
	| Nil ->
		o "Nil"
and output_code(out: 'o OutputU.t)({bytecodes; locs}: code): unit =
	OutputU.out out "Code(%a, %a)"
		(OutputU.out_array output_bytecode) bytecodes
		CodeLocs.output locs
