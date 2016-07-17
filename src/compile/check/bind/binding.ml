type v =
	| External of N.v (* Either a builtin or an import. *)
	| VDeclared of Ast.decl_val
	| Local of Ast.local_declare
	| Parameter of Ast.parameter

type ty =
	| ExternalTy of N.ty
	| TDeclared of Ast.decl_ty
