type t =
	| Bool of bool
	| Float of float
	| Int of int
	| Record of Type.record * t array
	| Void

let typ = function
	| Bool _ -> Type.Bool
	| Int _ -> Type.Int
	| Float _ -> Type.Float
	| Record(typ, _) -> Type.Rec typ
	| Void -> Type.Void

(* If this is raised, the typechecker was wrong. *)
exception CastFail

let cast_as_bool = function
	| Bool b -> b
	| _ -> raise CastFail

let cast_as_int = function
	| Int i -> i
	| _ -> raise CastFail

let cast_as_float = function
	| Float f -> f
	| _ -> raise CastFail

let rec output(out: 'a OutputU.t)(value: t): unit =
	match value with
	| Bool b ->
		OutputU.out out "%b" b
	| Int i ->
		OutputU.out out "%d" i
	| Float f ->
		OutputU.out out "%f" f
	(*TODO:{Type.rname = rname; Type.properties = props}*)
	| Record(record, properties) ->
		Symbol.output out record.Type.rname;
		OutputU.str out "(";
		let out_prop i type_property =
			let value = Array.get properties i in
			OutputU.out out "%a=%a, " Symbol.output type_property.Type.prop_name output value in
		(*TODO:ArrayU.iteri*)
		Array.iteri out_prop record.Type.properties;
		OutputU.str out ")"
	| Void ->
		OutputU.str out "void"
