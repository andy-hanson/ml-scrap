open N

(* If this is raised, the typechecker was wrong. *)
exception CastFail

let equal(a: v)(b: v): bool =
	a = b

let bool_of = function
	| Primitive(Bool b) -> b
	| _ -> raise CastFail
let int_of = function
	| Primitive(Int i) -> i
	| _ -> raise CastFail
let float_of = function
	| Primitive(Float f) -> f
	| _ -> raise CastFail

let ty_of_primitive = function
	| Bool _ -> TBool
	| Int _ -> TInt
	| Float _ -> TFloat
	| String _ -> TString
	| Void -> TVoid

let rec ty_of_fn(fn: fn): ft =
	match fn with
	| BuiltinFn {builtin_fn_ty; _} ->
		builtin_fn_ty
	| DeclaredFn {fn_ty; _} ->
		fn_ty
	| PartialFn {partially_applied; partial_args} ->
		TyU.partial_ty (ty_of_fn partially_applied) @@ ArrayU.map partial_args ty_of
	| Ctr _ ->
		raise U.TODO (*TODO: I left a ctr_type helper around somewhere...*)

and ty_of(v: v): ty =
 	match v with
	| Primitive p ->
		TPrimitive(ty_of_primitive p)
	| Fn fn ->
		Ft(ty_of_fn fn)
	| Rc(rt, _) ->
		Rt rt

let fn_name({fn_ty = {fname; _}; _}: declared_fn): Sym.t =
	fname
let builtin_fn_name({builtin_fn_ty = {fname; _}; _}: builtin_fn): Sym.t =
	fname
let fn_arity({fn_ty; _}: declared_fn): int =
	TyU.ft_arity fn_ty

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
		o "DeclaredFn(%a)" Sym.output (fn_name fn)
	| BuiltinFn b ->
		o "BuiltinFn(%a)" Sym.output @@ builtin_fn_name b
	| PartialFn {partially_applied; partial_args} ->
		OutputU.out out "PartialFn(%a, %a)"
			output_fn partially_applied
			(ArrayU.output output) partial_args
	| Ctr {rname; _} ->
		OutputU.out out "Ctr(%a)"
			Sym.output rname

(*TODO: NOut module*)
and output(out: 'o OutputU.t)(value: v): unit =
	let o fmt = OutputU.out out fmt in
	match value with
	| Primitive p ->
		output_primitive out p
	| Fn f ->
		output_fn out f
	| Rc({rname; properties}, property_values) ->
		let out_property(out: 'o OutputU.t)(((name, _), value): N.property * v): unit =
			OutputU.out out "%a=%a"
				Sym.output name
				output value in
		o "%a(%a)"
			Sym.output rname
			(ArrayU.output_elements out_property) (ArrayU.zip properties property_values)

and output_declared_fn(out: 'o OutputU.t)({fn_ty; _}: declared_fn): unit =
	OutputU.out out "Fn(%a)"
		TyU.output_ft fn_ty

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
		o "Cs(%a)" (ArrayU.output @@ OutputU.out_pair TyU.output_brief OutputU.output_int) parts
	| Const v ->
		o "Const(%a)" output v
	| CnvRc(rt, indices) ->
		o "CnvRc(%a, %a)"
			TyU.output_rt rt
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
