open Binding

let output_v(out: 'o OutputU.t)(b: v): unit =
	match b with
	| Builtin _ ->
		raise U.TODO (*OutputU.out out "Builtin(%a)" BuiltinU.output b*)
	| VDeclared d ->
		OutputU.out out "VDeclared(%a)" AstU.output_decl_val d
	| Local l ->
		OutputU.out out "Local(%a)" AstU.output_local_declare l
	| Parameter p ->
		OutputU.out out "Parameter(%a)" AstU.output_parameter p

let output_ty(out: 'o OutputU.t)(b: ty): unit =
	match b with
	| TDeclared d ->
		OutputU.out out "TDeclared(%a)" AstU.output_decl_ty d
	| BuiltinType b ->
		OutputU.out out "BuiltinType(%a)" TyU.output b
