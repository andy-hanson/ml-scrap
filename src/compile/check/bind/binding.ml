type t =
	| Builtin of Builtin.t
	| Declared of Ast.decl
	| Local of Ast.local_declare
	| Parameter of Ast.parameter
	| BuiltinType of N.ty
