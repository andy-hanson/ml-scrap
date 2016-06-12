type ctx =
	| Line
	| Paren
	| Case

type next =
	| Newline
	| Dedent
	| EndCase
	| CtxEnded

let parse_single(l: Lexer.t)(start: Loc.pos)(t: Token.t): Ast.expr =
	let loc = Lexer.loc_from l start in
	match t with
		| Token.Name name | Token.TypeName name ->
			Ast.ExprAccess(Ast.Access(loc, name))
		| Token.Literal value ->
			Ast.Literal(loc, value)
		| _ ->
			ParseU.unexpected start l t

let rec parse_expr_parts(l: Lexer.t)(next: Token.t)(ctx: ctx): Ast.expr list * next =
	(*TODO: this should be an input, taken before 'next'...*)
	let start = Lexer.pos l in
	match next with
	| Token.Case ->
		CompileErrorU.check (ctx = Line) (Lexer.loc_from l start) CompileError.CaseMustBeInLineContext;
		let expr = parse_case l start in
		[expr], EndCase
	| Token.Colon ->
		(* `:` groups the rest of the line into a single part *)
		let expr, next = parse_expr l ctx in
		[expr], next
	| Token.Lparen ->
		let a, next = parse_expr l Paren in
		assert (next = CtxEnded);
		let b, next = parse_expr_parts l (Lexer.next l) ctx in
		a :: b, next
	| Token.Newline | Token.Dedent ->
		begin match ctx with
		| Line ->
			[], if next = Token.Newline then Newline else Dedent
		| _ ->
			ParseU.unexpected start l next
		end
	| Token.Indent ->
		begin match ctx with
		| Case ->
			[], CtxEnded
		| _ ->
			raise U.TODO
			(*TODO: if in paren, fail. if in line, parse a block.*)
		end
	| Token.Rparen ->
		begin match ctx with
		| Paren ->
			[], CtxEnded
		| _ ->
			ParseU.unexpected start l next
		end
	| x ->
		let a = parse_single l start x in
		let b, next = parse_expr_parts l (Lexer.next l) ctx in
		a :: b, next

and parse_expr_with_next(l: Lexer.t)(next: Token.t)(ctx: ctx): Ast.expr * next =
	(*TODO: this should be an input, taken before 'next'...*)
	let start = Lexer.pos l in
	let parts, next = parse_expr_parts l next ctx in
	let loc = Lexer.loc_from l start in
	let expr = match parts with
	| [] ->
		CompileErrorU.raise loc CompileError.EmptyExpression
	| [part] ->
		part
	| hd::tl ->
		Ast.Call(loc, hd, Array.of_list tl) in
	expr, next

and parse_expr(l: Lexer.t)(ctx: ctx): Ast.expr * next =
	parse_expr_with_next l (Lexer.next l) ctx

(*TODO:MOVE?*)
and parse_case(l: Lexer.t)(start: Loc.pos): Ast.expr =
	let cased, next = parse_expr l Case in
	assert (next = CtxEnded);
	let try_parse_part() =
		let start, next = Lexer.pos_next l in
		match next with
		| Token.Dedent ->
			None
		| Token.Colon ->
			(* For now, only kind of case is :name Type *)
			let declare = parse_local_declare l in
			let typ = ParseType.f l in
			let test = Ast.AsTest((Lexer.loc_from l start), declare, typ) in
			ParseU.must_skip l Token.Indent;
			let result = f l in
			Some(Ast.CasePart((Lexer.loc_from l start), test, result))
		| x ->
			ParseU.unexpected start l x in
	let parts = ArrayU.build_until_none try_parse_part in
	Ast.Case((Lexer.loc_from l start), cased, parts)

and parse_local_declare(l: Lexer.t): Ast.local_declare =
	let start = Lexer.pos l in
	let name = ParseU.parse_name l in
	Ast.LocalDeclare((Lexer.loc_from l start), name)

and parse_block(l: Lexer.t)(start: Loc.pos)(first: Token.t): Ast.expr =
	match first with
	| Token.Colon ->
		let declare = parse_local_declare l in
		let expr, next = parse_expr l Line in
		CompileErrorU.check (next != Dedent) (Lexer.loc_from l start) CompileError.BlockCantEndInDeclare;
		let rest = f l in
		let loc = Lexer.loc_from l start in
		Ast.Let(loc, declare, expr, rest)
	| x ->
		let expr, next = parse_expr_with_next l x Line in
		begin match next with
		| Newline ->
			let rest = f l in
			let loc = Lexer.loc_from l start in
			Ast.Seq(loc, expr, rest)
		| EndCase ->
			let start, first = Lexer.pos_next l in
			begin match first with
			| Token.Dedent ->
				expr
			| _ ->
				let rest = parse_block l start first in
				let loc = Lexer.loc_from l start in (*TODO:duplicate of above*)
				Ast.Seq(loc, expr, rest)
			end
		| Dedent ->
			expr
		| CtxEnded ->
			(*We shouldn't ever get CtxEnded for the Line context*)
			assert false
		end

and f(l: Lexer.t): Ast.expr =
	let start, next = Lexer.pos_next l in
	parse_block l start next
