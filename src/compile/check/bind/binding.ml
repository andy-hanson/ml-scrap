type v =
	| Builtin of N.v
	(*TODO: ast.decl_val*)
	| VDeclared of Ast.decl
	| Local of Ast.local_declare
	| Parameter of Ast.parameter

type ty =
	| BuiltinType of N.ty
	(*TODO: ast.decl_ty*)
	| TDeclared of Ast.decl
