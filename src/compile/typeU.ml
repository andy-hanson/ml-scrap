let builtin_name = function
	| Type.Bool -> "Bool"
	| Type.Float -> "Float"
	| Type.Int -> "Int"
	| Type.Void -> "Void"
	| _ -> assert false

let fn(return_type: Type.t)(parameters: Type.t array): Type.fn =
	{ Type.return_type; Type.parameters }

let t_fn(return_type: Type.t)(parameters: Type.t array): Type.t =
	Type.Fn (fn return_type parameters)

let rc_arity({Type.properties; _}: Type.rc): int =
	Array.length properties

let rec output_property(out: 'o OutputU.t)({Type.prop_name; Type.prop_type}: Type.property): unit =
	OutputU.out out "Property(%a, %a)" Symbol.output prop_name output prop_type

and output_fn(out: 'o OutputU.t)({Type.return_type; Type.parameters}: Type.fn): unit =
	OutputU.out out "Fn(%a, %a)" output return_type (OutputU.out_array output) parameters

and output_rc(out: 'o OutputU.t)({Type.rname; Type.properties}: Type.rc): unit =
	OutputU.out out "Record(%a, %a)" Symbol.output rname (OutputU.out_array output_property) properties

and output(out: 'o OutputU.t)(t: Type.t): unit =
	match t with
	| Type.Fn fn ->
		output_fn out fn
	| Type.Rc rc ->
		output_rc out rc
	| Type.Or parts ->
		OutputU.out out "Or(%a)" (OutputU.out_array output) parts
	| builtin ->
		OutputU.str out (builtin_name builtin)

(* Dynamic type test *)
let rec subsumes(t: Type.t)(v: Val.t): bool =
	match t with
	| Type.Bool ->
		begin match v with
		| Val.Bool _ -> true
		| _ -> false
		end
	| Type.Float ->
		begin match v with
		| Val.Float _ -> true
		| _ -> false
		end
	| Type.Int ->
		begin match v with
		| Val.Int _ -> true
		| _ -> false
		end
	| Type.Void ->
		begin match v with
		| Val.Void -> true
		| _ -> false
		end
	| Type.Fn _ ->
		raise U.TODO
	| Type.Or parts ->
		ArrayU.exists parts (fun p -> subsumes p v)
	| Type.Rc rc ->
		begin match v with
		| Val.Rc(val_rc, _) -> rc == val_rc
		| _ -> false
		end

let check_subsumes(t: Type.t)(v: Val.t): unit =
	if not (subsumes t v) then
		failwith (OutputU.out_to_string "Expected a %a, got %a" output t ValU.output v)

let assert_subsumes = check_subsumes
