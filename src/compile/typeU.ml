open Type

let name = function
	| Any -> Sym.of_string "Any"
	| Bool -> Sym.of_string "Bool"
	| Float -> Sym.of_string "Float"
	| Int -> Sym.of_string "Int"
	| Void -> Sym.of_string "Void"
	| Ft {fname; _} -> fname
	| Rc {rname; _} -> rname
	| Un {uname; _} -> uname

let or_is_subtype(_a_types: t array)(_b_types: t array): bool =
	raise U.TODO

let rec is_subtype(a: t)(b: t): bool =
	match a with
	| Any ->
		true
	| Bool | Float | Int | Void | Rc _ ->
		a = b
	| Ft {fname = _; return_type; parameters} ->
		begin match b with
		| Ft {fname = _; return_type = b_return_type; parameters = b_parameters} ->
			is_subtype return_type b_return_type &&
				(* Functions are contravariant in parameter types *)
				ArrayU.for_all_zip b_parameters parameters begin fun (_, param_type_b) (_, param_type) ->
					is_subtype param_type_b param_type
				end
		| _ ->
			false
		end
	| Un {types = a_types; _} ->
		begin match b with
		| Un {types = b_types; _} ->
			or_is_subtype a_types b_types
		| Bool | Float | Int | Void | Rc _ ->
			(* A union type is guaranteed to only contain simple types. *)
			ArrayU.exists a_types ((=) b)
		| Any | Ft _ ->
			false
		end

let ft(fname: Sym.t)(return_type: t)(parameters: parameter array): ft =
	{fname; return_type; parameters}

let t_ft(fname: Sym.t)(return_type: t)(parameters: parameter array): t =
	Ft (ft fname return_type parameters)

let property(prop_name: Sym.t)(prop_type: t): property =
	{prop_name; prop_type}
let t_rc(rname: Sym.t)(properties: property array): t =
	Rc {rname; properties}

let ft_arity({parameters; _}: ft): int =
	Array.length parameters
let rc_arity({properties; _}: rc): int =
	Array.length properties

let rec output_property(out: 'o OutputU.t)({prop_name; prop_type}: property): unit =
	OutputU.out out "Property(%a, %a)" Sym.output prop_name output prop_type

and output_ft(out: 'o OutputU.t)({fname; return_type; parameters}: ft): unit =
	let output_parameter(out: 'o OutputU.t)((name, typ): parameter): unit =
		OutputU.out out "%a %a"
			Sym.output name
			output typ in
	OutputU.out out "Fn(%a, %a, %a)"
		Sym.output fname
		output return_type
		(OutputU.out_array output_parameter) parameters

and output_rc(out: 'o OutputU.t)({rname; properties}: rc): unit =
	OutputU.out out "Record(%a, %a)" Sym.output rname (OutputU.out_array output_property) properties

and output(out: 'o OutputU.t)(t: t): unit =
	match t with
	| Ft ft ->
		output_ft out ft
	| Rc rc ->
		output_rc out rc
	| Un {uname; types} ->
		OutputU.out out "Un(%a, %a)"
			Sym.output uname
			(OutputU.out_array output) types
	| Any | Bool | Float | Int | Void ->
		OutputU.str out @@ Sym.string_of @@ name t
