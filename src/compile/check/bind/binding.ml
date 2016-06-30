type t =
	| Builtin of N.v
	| Declared of Ast.decl
	| Local of Ast.local_declare
	| Parameter of Ast.parameter
	| BuiltinType of N.ty
