let parse_single(p: Parser.t)(start: Loc.pos)(t: Token.t): Ast.expr =
	let expr = match t with
		| Token.Name name | Token.TypeName name ->
			Ast.Access name
		| Token.Literal value ->
			Ast.Literal value
		| _ ->
			ParseU.unexpected p t in
	Ast.Expr(Parser.loc_from p start, expr)

let rec parse_expr_parts(p: Parser.t)(next: Token.t)(in_paren: bool): Ast.expr list * bool =
	let start = Parser.pos p in
	match next with
	| x when not in_paren && (x = Token.Newline || x = Token.Dedent) ->
		[], x = Token.Newline
	| Token.Rparen when in_paren ->
		[], false (* next_is_newline isn't used, so ignore *)
	| Token.Case ->
		raise U.TODO
	| Token.Colon ->
		let expr, next_is_newline = parse_expr p in_paren in
		[expr], next_is_newline
	| Token.Lparen ->
		(* Ignore next_is_newline for parenthesized part *)
		let a, _ = parse_expr p true in
		let b, next_is_newline = parse_expr_parts p (Parser.next p) in_paren in
		a :: b, next_is_newline
	| x ->
		let a = parse_single p start x in
		let b, next_is_newline = parse_expr_parts p (Parser.next p) in_paren in
		a :: b, next_is_newline

(* Returns true if next is newline, else false *)
and parse_expr_with_next(p: Parser.t)(next: Token.t)(in_paren: bool): Ast.expr * bool =
	let start = Parser.pos p in
	let parts, next_is_newline = parse_expr_parts p next in_paren in
	let loc = Parser.loc_from p start in
	let expr = match parts with
	| [] ->
		CompileError.raise loc CompileError.EmptyExpression
	| [part] ->
		part
	| hd::tl ->
		Ast.Expr(loc, Ast.Call(hd, Array.of_list tl)) in
	expr, next_is_newline

and parse_expr(p: Parser.t)(in_paren: bool): Ast.expr * bool =
	parse_expr_with_next p (Parser.next p) in_paren

let rec parse_block(p: Parser.t): Ast.expr =
	let start = Parser.pos p in
	match Parser.next p with
	| Token.Colon ->
		(*TODO:neater*)
		let start' = Parser.pos p in
		let name = ParseU.parse_name p in
		let name_loc = Parser.loc_from p start' in

		let declare = Ast.LocalDeclare(name_loc, name) in

		let expr, next_is_newline = parse_expr p false in

		let loc = Parser.loc_from p start in
		CompileError.check next_is_newline loc CompileError.BlockCantEndInDeclare;
		let rest = parse_block p in
		Ast.Expr(loc, Ast.Let(declare, expr, rest))
	| x ->
		let expr, next_is_newline = parse_expr_with_next p x false in
		if next_is_newline then
			let loc = Parser.loc_from p start in
			let rest = parse_block p in
			Ast.Expr(loc, Ast.Seq(expr, rest))
		else
			expr
