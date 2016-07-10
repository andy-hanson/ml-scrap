type v =
	| Builtin of N.v
	| VDeclared of Ast.decl_val
	| Local of Ast.local_declare
	| Parameter of Ast.parameter

type ty =
	| BuiltinType of N.ty
	| TDeclared of Ast.decl_ty
