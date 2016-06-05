let parse_single(p: Parser.t)(start: Loc.pos)(t: Token.t): Ast.expr =
	let expr = match t with
		| Token.Name name | Token.TypeName name ->
			Ast.Access name
		| Token.Literal value ->
			Ast.Literal value
		| _ ->
			ParseU.unexpected p t in
	Ast.Expr(Parser.loc_from p start, expr)

let rec parse_expr_parts(p: Parser.t)(in_paren: bool): Ast.expr list * bool =
	let start = Parser.pos p in
	match Parser.next p with
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
		let b, next_is_newline = parse_expr_parts p in_paren in
		a :: b, next_is_newline
	| x ->
		let a = parse_single p start x in
		let b, next_is_newline = parse_expr_parts p in_paren in
		a :: b, next_is_newline

(* Returns true if next is newline, else false *)
and parse_expr(p: Parser.t)(in_paren: bool): Ast.expr * bool =
	let start = Parser.pos p in
	let parts, next_is_newline = parse_expr_parts p in_paren in
	let loc = Parser.loc_from p start in
	let expr = match parts with
	| [] ->
		CompileError.raise loc CompileError.EmptyExpression
	| [part] ->
		part
	| hd::tl ->
		Ast.Expr(loc, Ast.Call(hd, Array.of_list tl)) in
	expr, next_is_newline

(*TODO:NEATER*)
let find_eq(p: Parser.t): bool =
	Parser.with_restore p (fun () ->
		U.loop_until_result (fun () ->
			match Parser.next p with
			| Token.Equals ->
				Some true
			| Token.Newline | Token.Dedent ->
				Some false
			| _ ->
				None))

let rec parse_block(p: Parser.t): Ast.expr =
	let start = Parser.pos p in
	if find_eq p then begin
		let declare = ParseLocalDeclare.f p in
		ParseU.must_skip p Token.Equals;
		let expr, next_is_newline = parse_expr p false in
		let loc = Parser.loc_from p start in
		CompileError.check next_is_newline loc CompileError.BlockCantEndInDeclare;
		let rest = parse_block p in
		Ast.Expr(loc, Ast.Let(declare, expr, rest))
	end else
		let expr, next_is_newline = parse_expr p false in
		if next_is_newline then
			let loc = Parser.loc_from p start in
			let rest = parse_block p in
			Ast.Expr(loc, Ast.Seq(expr, rest))
		else
			expr
