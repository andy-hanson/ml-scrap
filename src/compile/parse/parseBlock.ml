type ctx =
	| Line
	| ExprOnly (* Like Line, but forbid an `=` because we're already in one *)
	| Paren (* Parse an expression and expect a ')' *)
	| CaseHead

type next =
	| NewlineAfterEquals of Ast.local_declare
	| NewlineAfterStatement
	| EndNestedBlock
	| CtxEnded

let parse_single(l: Lexer.t)(start: Loc.pos)(t: Token.t): Ast.expr =
	let loc = Lexer.loc_from l start in
	match t with
		| Token.Name name | Token.TypeName name ->
			Ast.ExprAccess(loc, name)
		| Token.Literal value ->
			Ast.Literal(loc, value)
		| _ ->
			ParseU.unexpected start l t

let rec parse_expr_with_next(l: Lexer.t)(expr_start: Loc.pos)(next: Token.t)(ctx: ctx): Ast.expr * next =
	let parts: Ast.expr MutArray.t = MutArray.create() in
	let add_part = MutArray.add parts in

	let finish_regular(): Ast.expr =
		let loc = Lexer.loc_from l expr_start in
		match MutArray.length parts with
		| 0 ->
			CompileErrorU.raise loc CompileError.EmptyExpression
		| 1 ->
			MutArray.get parts 0
		| _ ->
			let head = MutArray.get parts 0 in
			let tail = MutArray.tail parts in
			Ast.Call(loc, head, tail) in

	let rec recur(start: Loc.pos)(next: Token.t) =
		let unexpected() = ParseU.unexpected start l next in
		match next with
		| Token.Case ->
			CompileErrorU.check (ctx = Line) (Lexer.loc_from l start) CompileError.CaseMustBeInLineContext;
			let expr = parse_case l start in
			add_part expr;
			finish_regular(), EndNestedBlock

		| Token.Colon ->
			let expr, next = parse_expr l ctx in
			add_part expr;
			finish_regular(), next

		| Token.Equals ->
			CompileErrorU.check (ctx = Line) (Lexer.loc_from l start) CompileError.EqualsInExpression;
			begin match MutArray.length parts with
			| 1 ->
				let x = MutArray.get parts 0 in
				begin match x with
				| Ast.ExprAccess(loc, name) ->
					let declare = loc, name in
					let expr, next = parse_expr l ExprOnly in
					CompileErrorU.check (next != CtxEnded) (Lexer.loc_from l start) CompileError.BlockCantEndInDeclare;
					assert (next = NewlineAfterStatement);
					expr, NewlineAfterEquals declare
				| _ ->
					CompileErrorU.raise (AstU.expr_loc x) CompileError.PrecedingEquals
				end
			| _ ->
				CompileErrorU.raise (Lexer.loc_from l start) CompileError.PrecedingEquals
			end

		| Token.Operator name ->
			let left = finish_regular() in
			let op = Ast.ExprAccess(Lexer.loc_from l start, name) in
			let right, next = parse_expr l ctx in
			Ast.Call(Lexer.loc_from l expr_start, op, [|left; right|]), next

		| Token.Lparen ->
			let a, next = parse_expr l Paren in
			add_part a;
			assert (next = CtxEnded);
			let start, next = Lexer.pos_next l in
			recur start next

		| Token.Rparen ->
			finish_regular(), begin match ctx with
			| Paren -> CtxEnded
			| _ -> unexpected()
			end

		| Token.Newline | Token.Dedent ->
			finish_regular(), begin match ctx with
			| Line | ExprOnly -> if next = Token.Newline then NewlineAfterStatement else CtxEnded
			| _ -> unexpected()
			end

		| Token.Indent ->
			begin match ctx with
			| CaseHead ->
				finish_regular(), CtxEnded
			| Line ->
				let start, next = Lexer.pos_next l in
				let expr = parse_block l start next in
				add_part expr;
				finish_regular(), EndNestedBlock
			| _ ->
				unexpected()
			end

		| x ->
			add_part @@ parse_single l start x;
			let start, next = Lexer.pos_next l in
			recur start next in

	recur expr_start next

and parse_expr(l: Lexer.t)(ctx: ctx): Ast.expr * next =
	let start, next = Lexer.pos_next l in
	parse_expr_with_next l start next ctx

and parse_case_parts(l: Lexer.t): Ast.case_part array =
	ArrayU.build_until_none begin fun () ->
		let start, next = Lexer.pos_next l in
		match next with
		| Token.Dedent ->
			None
		| Token.Name name ->
			let declare = Lexer.loc_from l start, name in
			let typ = ParseType.f l in
			let test = Ast.AsTest(Lexer.loc_from l start, declare, typ) in
			ParseU.must_skip l Token.Indent;
			let result = f l in
			Some(Lexer.loc_from l start, test, result)
		| x ->
			ParseU.unexpected start l x
	end

and parse_case(l: Lexer.t)(start: Loc.pos): Ast.expr =
	let cased, next = parse_expr l CaseHead in
	assert (next = CtxEnded);
	Ast.Case((Lexer.loc_from l start), cased, parse_case_parts l)

and parse_block(l: Lexer.t)(start: Loc.pos)(first: Token.t): Ast.expr =
	let expr, next = parse_expr_with_next l start first Line in
	begin match next with
	| NewlineAfterEquals declare ->
		let rest = f l in
		let loc = Lexer.loc_from l start in
		Ast.Let(loc, declare, expr, rest)
	| NewlineAfterStatement ->
		let rest = f l in
		let loc = Lexer.loc_from l start in
		Ast.Seq(loc, expr, rest)
	| EndNestedBlock ->
		let start, first = Lexer.pos_next l in
		begin match first with
		| Token.Dedent ->
			expr
		| _ ->
			let rest = parse_block l start first in
			let loc = Lexer.loc_from l start in (*TODO:duplicate of above*)
			Ast.Seq(loc, expr, rest)
		end
	| CtxEnded ->
		expr
	end

and f(l: Lexer.t): Ast.expr =
	let start, next = Lexer.pos_next l in
	parse_block l start next
