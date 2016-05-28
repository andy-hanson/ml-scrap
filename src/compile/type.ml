type builtin =
	| Bool
	| Int

let builtins = [| Bool; Int |]

let builtin_name = function
	| Bool -> "Bool"
	| Int -> "Int"

type t =
	| Builtin of builtin
	| Rec of record
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

