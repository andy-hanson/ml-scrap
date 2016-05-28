type t =
	| Builtin of Builtins.builtin
	| Declared of Ast.decl_val
	| Local of Ast.local_declare
	| BuiltinType of Type.builtin
	| DeclaredType of Ast.decl_type

(* boilerplate*)

let output(out: 'a BatIO.output)(b: t): unit =
	match b with
	| Builtin b ->
		OutputU.out out "Builtin(%a)" Builtins.output b
	| Declared d ->
		OutputU.out out "Declared(%a)" Ast.output_decl_val d
	| Local l ->
		OutputU.out out "Local(%a)" Ast.output_local_declare l
	| BuiltinType b ->
		OutputU.out out "Builtin(%a)" Type.output_builtin b
	| DeclaredType d ->
		OutputU.out out "Declared(%a)" Ast.output_decl_type d

