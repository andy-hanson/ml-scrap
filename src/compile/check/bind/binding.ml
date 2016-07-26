type v =
	| External of N.V.v (* Either a builtin or an import. *)
	| VDeclared of Ast.decl_val
	| Local of Ast.local_declare
	| Parameter of Ast.parameter

type ty =
	| ExternalTy of N.Ty.ty
	| TDeclared of Ast.decl_ty
	| TParameter of N.Ty.gen_var
