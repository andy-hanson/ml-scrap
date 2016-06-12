open Val

let typ = function
	| Bool _ -> Type.Bool
	| Int _ -> Type.Int
	| Float _ -> Type.Float
	| Rc(typ, _) -> Type.Rc typ
	| Void -> Type.Void

(* If this is raised, the typechecker was wrong. *)
exception CastFail

let bool_of = function
	| Bool b -> b
	| _ -> raise CastFail
let int_of = function
	| Int i -> i
	| _ -> raise CastFail
let float_of = function
	| Float f -> f
	| _ -> raise CastFail

let rec output(out: 'o OutputU.t)(value: t): unit =
	match value with
	| Bool b ->
		OutputU.out out "%b" b
	| Int i ->
		OutputU.out out "%d" i
	| Float f ->
		OutputU.out out "%f" f
	| Rc(record, properties) ->
		Symbol.output out record.Type.rname;
		OutputU.str out "(";
		ArrayU.iteri record.Type.properties begin fun i type_property ->
			let value = properties.(i) in
			OutputU.out out "%a=%a, " Symbol.output type_property.Type.prop_name output value
		end;
		OutputU.str out ")"
	| Void ->
		OutputU.str out "void"
