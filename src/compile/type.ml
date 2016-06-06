(*TODO: just merge these into `t`*)
type builtin =
	| Bool
	| Float
	| Int
	| Void

let builtins = [| Bool; Float; Int; Void |]

let builtin_name = function
	| Bool -> "Bool"
	| Float -> "Float"
	| Int -> "Int"
	| Void -> "Void"

type t =
	| PendingTypeCheck
	| Builtin of builtin
	| Fn of fn
	| Rec of record
	(* | Generic of generic_parameter array * t *)

and property = {
	prop_name: Symbol.t;
	prop_type: t
}

and fn = {
	return_type: t;
	parameters: t array
}

and record = {
	rname: Symbol.t;
	(* Mutable for sake of rec-creating algorithm *)
	mutable properties: property array
}

(*TODO: similar function for record*)
let fn(return_type: t)(parameters: t array): fn =
	{ return_type; parameters }

let t_fn(return_type: t)(parameters: t array): t =
	Fn (fn return_type parameters)

let record_arity(r: record) =
	Array.length r.properties

(* boilerplate *)

let output_builtin(out: 'a OutputU.t)(b: builtin): unit =
	OutputU.str out (builtin_name b)

let rec output_property(out: 'a OutputU.t)({prop_name; prop_type}: property): unit =
	OutputU.out out "Property(%a, %a)" Symbol.output prop_name output prop_type

and output_fn(out: 'a OutputU.t)({return_type; parameters}: fn): unit =
	OutputU.out out "Fn(%a, %a)" output return_type (OutputU.out_array output) parameters

and output_record(out: 'a OutputU.t)({rname; properties}: record): unit =
	OutputU.out out "Record(%a, %a)" Symbol.output rname (OutputU.out_array output_property) properties

and output(out: 'a OutputU.t)(t: t): unit =
	match t with
	| PendingTypeCheck ->
		(*TODO:replace all nwrite with str*)
		OutputU.str out "PENDING TYPE CHECK"
	| Builtin b ->
		output_builtin out b
	| Fn fn ->
		output_fn out fn
	| Rec r ->
		output_record out r
