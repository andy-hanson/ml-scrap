type t =
	| Bool
	| Float
	| Int
	| Void
	| PendingTypeCheck (*TODO:KILL*)
	| Fn of fn
	| Rc of rc

and property = {
	prop_name: Symbol.t;
	prop_type: t
}

and fn = {
	return_type: t;
	parameters: t array
}

and rc = {
	rname: Symbol.t;
	(* Mutable for sake of rec-creating algorithm *)
	mutable properties: property array
}

let builtins = [| Bool; Float; Int; Void |]

let builtin_name = function
	| Bool -> "Bool"
	| Float -> "Float"
	| Int -> "Int"
	| Void -> "Void"
	| _ -> assert false

(*TODO: similar function for record*)
let fn(return_type: t)(parameters: t array): fn =
	{ return_type; parameters }

(*TODO:RENAME*)
let t_fn(return_type: t)(parameters: t array): t =
	Fn (fn return_type parameters)

(*TODO:TypeU.ml*)
let rc_arity({properties; _}: rc) =
	Array.length properties

(* boilerplate *)

let rec output_property(out: 'o OutputU.t)({prop_name; prop_type}: property): unit =
	OutputU.out out "Property(%a, %a)" Symbol.output prop_name output prop_type

and output_fn(out: 'o OutputU.t)({return_type; parameters}: fn): unit =
	OutputU.out out "Fn(%a, %a)" output return_type (OutputU.out_array output) parameters

and output_rc(out: 'o OutputU.t)({rname; properties}: rc): unit =
	OutputU.out out "Record(%a, %a)" Symbol.output rname (OutputU.out_array output_property) properties

and output(out: 'o OutputU.t)(t: t): unit =
	match t with
	| PendingTypeCheck ->
		(*TODO:replace all nwrite with str*)
		OutputU.str out "PENDING TYPE CHECK"
	| Fn fn ->
		output_fn out fn
	| Rc rc ->
		output_rc out rc
	| builtin ->
		OutputU.str out (builtin_name builtin)
