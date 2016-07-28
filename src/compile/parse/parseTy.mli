val parse_gen_inst: Lexer.t -> Ast.ty array
(* inline needs `[]` as in `[Foo Bar]`, free does not need `[]` because it ends with a different token. *)
val inline: Lexer.t -> Ast.ty
val inline_with_start: Lexer.t -> Loc.pos -> Token.t -> Ast.ty
val free: Lexer.t -> Ast.ty * Loc.pos * Token.t
val parse_ty_name_or_generic: Lexer.t -> Ast.fn_head
