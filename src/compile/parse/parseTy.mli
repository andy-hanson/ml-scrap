val parse_gen_inst: Lexer.t -> Ast.ty array
val f: Lexer.t -> Ast.ty
val f_with_start: Lexer.t -> Loc.pos -> Token.t -> Ast.ty
val parse_ty_name_or_generic: Lexer.t -> Ast.ty_name
