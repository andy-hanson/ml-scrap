open Binding

let output(out: 'o OutputU.t)(b: t): unit =
	match b with
	| Builtin _ ->
		raise U.TODO (*OutputU.out out "Builtin(%a)" BuiltinU.output b*)
	| Declared d ->
		OutputU.out out "Declared(%a)" AstU.output_decl d
	| Local l ->
		OutputU.out out "Local(%a)" AstU.output_local_declare l
	| Parameter p ->
		OutputU.out out "Parameter(%a)" AstU.output_parameter p
	| BuiltinType b ->
		OutputU.out out "BuiltinType(%a)" TyU.output b
