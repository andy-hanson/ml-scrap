(*TODO: maybe just merge these into `t`*)
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
	| Fn of t * t array
	| Rec of record
	(* | Generic of generic_parameter array * t *)

and property = {
	prop_name: Symbol.t;
	prop_type: t
}

and record = {
	rname: Symbol.t;
	(* Mutable for sake of rec-creating algorithm *)
	mutable properties: property array
}

let record_arity(r: record) =
	Array.length r.properties

(* boilerplate *)

let output_builtin(out: 'a BatIO.output)(b: builtin): unit =
	BatIO.nwrite out (builtin_name b)

let rec output_property(out: 'a BatIO.output)({prop_name; prop_type}: property): unit =
	OutputU.out out "Property(%a, %a)" Symbol.output prop_name output prop_type

and output_record(out: 'a BatIO.output)({rname; properties}: record): unit =
	OutputU.out out "Record(%a, %a)" Symbol.output rname (OutputU.out_array output_property) properties

and output(out: 'a BatIO.output)(t: t): unit =
	match t with
	| PendingTypeCheck ->
		(*TODO:replace all nwrite with str*)
		OutputU.str out "PENDING TYPE CHECK"
	| Builtin b ->
		output_builtin out b
	| Fn(r, args) ->
		OutputU.out out "Fn(%a, %a)" output r (OutputU.out_array output) args
	| Rec r ->
		output_record out r
