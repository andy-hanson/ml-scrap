open N

let name = function
	| Any -> Sym.of_string "Any"
	| TBool -> Sym.of_string "Bool"
	| TFloat -> Sym.of_string "Float"
	| TInt -> Sym.of_string "Int"
	| TVoid -> Sym.of_string "Void"
	| Ft {fname; _} -> fname
	| Rt {rname; _} -> rname
	| Un {uname; _} -> uname
	| Ct {cname; _} -> cname

(*TODO: union_is_subtype*)
let or_is_subtype(_a_types: ty array)(_b_types: ty array): bool =
	raise U.TODO

(*TODO: reverse argument order... should be "a is subtype of b"*)
(*And make this the `<:` operator*)
let rec is_subtype(a: ty)(b: ty): bool =
	match a with
	| Any ->
		true
	| TBool | TFloat | TInt | TVoid | Rt _ ->
		a = b
	| Un {utypes = a_types; _} ->
		begin match b with
		| Un {utypes = b_types; _} ->
			or_is_subtype a_types b_types
		| TBool | TFloat | TInt | TVoid | Rt _ ->
			(* A union type is guaranteed to only contain simple types. *)
			ArrayU.exists a_types ((=) b)
		| Any | Ft _ | Ct _ ->
			false
		end
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
	| Ct _ ->
		begin match b with
		| Ct _ ->
			a == b
			(* TODO: b must accept a superset of arguments and have a subset of return values. *)
		| Ft _ ->
			(* TODO: must take one argument, and be a subtype of a case in A*)
			raise U.TODO
		| Any | TBool | TFloat | TInt | TVoid | Rt _ | Un _ ->
			false
		end

let ft(fname: Sym.t)(return_type: ty)(parameters: parameter array): ft =
	{fname; return_type; parameters}

let t_ft(fname: Sym.t)(return_type: ty)(parameters: parameter array): ty =
	Ft(ft fname return_type parameters)

let t_rc(rname: Sym.t)(properties: property array): ty =
	Rt {rname; properties}

let ft_arity({parameters; _}: ft): int =
	Array.length parameters
let rt_arity({properties; _}: rt): int =
	Array.length properties
let arity(typ: ty): int =
	match typ with
	| Ft f -> ft_arity f
	| Ct _ -> 1
	| _ -> assert false

let rec output_property(out: 'o OutputU.t)((name, typ): property): unit =
	OutputU.out out "Property(%a, %a)"
		Sym.output name
		output typ

and output_ft(out: 'o OutputU.t)({fname; return_type; parameters}: ft): unit =
	let output_parameter(out: 'o OutputU.t)((name, typ): parameter): unit =
		OutputU.out out "%a %a"
			Sym.output name
			output typ in
	OutputU.out out "Fn(%a, %a, %a)"
		Sym.output fname
		output return_type
		(OutputU.out_array output_parameter) parameters

and output_ct(_out: 'o OutputU.t)({cname = _; ct_cases = _}: ct): unit =
	raise U.TODO

and output_fn_type(_out: 'o OutputU.t)(_fn_type: fn_type): unit =
	raise U.TODO

and output_rc(out: 'o OutputU.t)({rname; properties}: rt): unit =
	OutputU.out out "Record(%a, %a)" Sym.output rname (OutputU.out_array output_property) properties

and output(out: 'o OutputU.t)(t: ty): unit =
	match t with
	| Any | TBool | TFloat | TInt | TVoid ->
		OutputU.str out @@ Sym.string_of @@ name t
	| Rt rc ->
		output_rc out rc
	| Un {uname; utypes} ->
		OutputU.out out "Un(%a, %a)"
			Sym.output uname
			(OutputU.out_array output) utypes
	| Ft ft ->
		output_ft out ft
	| Ct {cname; ct_cases} ->
		let out_case(out: 'o OutputU.t)((return, input): ty * ty): unit =
			OutputU.out out "%a %a" output return output input in
		OutputU.out out "Ct(%a, %a)"
			Sym.output cname
			(OutputU.out_array out_case) ct_cases
