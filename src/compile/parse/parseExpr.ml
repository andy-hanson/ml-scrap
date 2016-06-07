let parse_single(l: Lexer.t)(start: Loc.pos)(t: Token.t): Ast.expr =
	let expr = match t with
		| Token.Name name | Token.TypeName name ->
			Ast.Access name
		| Token.Literal value ->
			Ast.Literal value
		| _ ->
			ParseU.unexpected l t in
	Ast.Expr(Lexer.loc_from l start, expr)

let rec parse_expr_parts(l: Lexer.t)(next: Token.t)(in_paren: bool): Ast.expr list * bool =
	let start = Lexer.pos l in
	match next with
	| x when not in_paren && (x = Token.Newline || x = Token.Dedent) ->
		[], x = Token.Newline
	| Token.Rparen when in_paren ->
		[], false (* next_is_newline isn't used, so ignore *)
	| Token.Case ->
		raise U.TODO
	| Token.Colon ->
		let expr, next_is_newline = parse_expr l in_paren in
		[expr], next_is_newline
	| Token.Lparen ->
		(* Ignore next_is_newline for parenthesized part *)
		let a, _ = parse_expr l true in
		let b, next_is_newline = parse_expr_parts l (Lexer.next l) in_paren in
		a :: b, next_is_newline
	| x ->
		let a = parse_single l start x in
		let b, next_is_newline = parse_expr_parts l (Lexer.next l) in_paren in
		a :: b, next_is_newline

(* Returns true if next is newline, else false *)
and parse_expr_with_next(l: Lexer.t)(next: Token.t)(in_paren: bool): Ast.expr * bool =
	let start = Lexer.pos l in
	let parts, next_is_newline = parse_expr_parts l next in_paren in
	let loc = Lexer.loc_from l start in
	let expr = match parts with
	| [] ->
		CompileError.raise loc CompileError.EmptyExpression
	| [part] ->
		part
	| hd::tl ->
		Ast.Expr(loc, Ast.Call(hd, Array.of_list tl)) in
	expr, next_is_newline

and parse_expr(l: Lexer.t)(in_paren: bool): Ast.expr * bool =
	parse_expr_with_next l (Lexer.next l) in_paren

let rec parse_block(l: Lexer.t): Ast.expr =
	let start = Lexer.pos l in
	match Lexer.next l with
	| Token.Colon ->
		(*TODO:neater*)
		let start' = Lexer.pos l in
		let name = ParseU.parse_name l in
		let name_loc = Lexer.loc_from l start' in

		let declare = Ast.LocalDeclare(name_loc, name) in

		let expr, next_is_newline = parse_expr l false in

		let loc = Lexer.loc_from l start in
		CompileError.check next_is_newline loc CompileError.BlockCantEndInDeclare;
		let rest = parse_block l in
		Ast.Expr(loc, Ast.Let(declare, expr, rest))
	| x ->
		let expr, next_is_newline = parse_expr_with_next l x false in
		if next_is_newline then
			let loc = Lexer.loc_from l start in
			let rest = parse_block l in
			Ast.Expr(loc, Ast.Seq(expr, rest))
		else
			expr
