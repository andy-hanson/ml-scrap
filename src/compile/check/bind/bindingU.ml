open Binding

let output_v(out: 'o OutputU.t)(b: v): unit =
	match b with
	| External v ->
		OutputU.out out "External(%a)" ValOut.output v
	| VDeclared d ->
		OutputU.out out "VDeclared(%a)" AstOut.output_decl_val d
	| Local l ->
		OutputU.out out "Local(%a)" AstOut.output_local_declare l
	| Parameter p ->
		OutputU.out out "Parameter(%a)" AstOut.output_parameter p

let output_ty(out: 'o OutputU.t)(b: ty): unit =
	match b with
	| ExternalTy ty ->
		OutputU.out out "ExternalTy(%a)" TyOut.output ty
	| TDeclared d ->
		OutputU.out out "TDeclared(%a)" AstOut.output_decl_ty d
	| TParameter p ->
		OutputU.out out "TParameter(%a)" AstOut.output_ty_param p
