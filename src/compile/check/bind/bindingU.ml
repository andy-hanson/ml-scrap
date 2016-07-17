open Binding

let output_v(out: 'o OutputU.t)(b: v): unit =
	match b with
	| External v ->
		OutputU.out out "External(%a)" ValU.output v
	| VDeclared d ->
		OutputU.out out "VDeclared(%a)" AstU.output_decl_val d
	| Local l ->
		OutputU.out out "Local(%a)" AstU.output_local_declare l
	| Parameter p ->
		OutputU.out out "Parameter(%a)" AstU.output_parameter p

let output_ty(out: 'o OutputU.t)(b: ty): unit =
	match b with
	| ExternalTy ty ->
		OutputU.out out "ExternalTy(%a)" TyU.output ty
	| TDeclared d ->
		OutputU.out out "TDeclared(%a)" AstU.output_decl_ty d
