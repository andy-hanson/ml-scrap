module type ExprParser = sig
	val parse_expr: Parser.t -> Ast.expr
end

module type DeclParser = sig
	val parse_decl: Parser.t -> Ast.decl
end

