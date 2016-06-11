type t =
	| Builtin of Builtins.builtin
	| Declared of Ast.decl
	| Local of Ast.local_declare
	| Parameter of Ast.parameter
	| BuiltinType of Type.t

(* boilerplate*)

let output(out: 'o OutputU.t)(b: t): unit =
	match b with
	| Builtin b ->
		OutputU.out out "Builtin(%a)" Builtins.output b
	| Declared d ->
		OutputU.out out "Declared(%a)" AstU.output_decl d
	| Local l ->
		OutputU.out out "Local(%a)" AstU.output_local_declare l
	| Parameter p ->
		OutputU.out out "Parameter(%a)" AstU.output_parameter p
	| BuiltinType b ->
		OutputU.out out "BuiltinType(%a)" Type.output b
