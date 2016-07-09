type ctx =
	| Line
	| ExprOnly (* Like Line, but forbid an `=` because we're already in one *)
	| Paren (* Parse an expression and expect a ')' *)
	| Quote (* Look for a QuoteEnd *)
	| CsHead

(* For debugging...*)
let output_ctx(out: 'o OutputU.t)(ctx: ctx): unit =
	OutputU.str out begin match ctx with
	| Line -> "Line"
	| ExprOnly -> "ExprOnly"
	| Paren -> "Paren"
	| Quote -> "Quote"
	| CsHead -> "CsHead"
	end

type next =
	| NewlineAfterEquals of Ast.local_declare
	| NewlineAfterStatement
	| EndNestedBlock
	| CtxEnded

let parse_single(l: Lexer.t)(start: Loc.pos)(t: Token.t): Ast.expr =
	let loc = Lexer.loc_from l start in
	match t with
		| Token.Name name (*| Token.TypeName name*) ->
			Ast.ExprAccess(loc, name)
		| Token.Literal value ->
			Ast.Literal(loc, value)
		| _ ->
			ParseU.unexpected start l t

(*TODO: neater*)
let rec parse_expr_with_next(l: Lexer.t)(expr_start: Loc.pos)(next: Token.t)(ctx: ctx): Ast.expr * next =
	let parts: Ast.expr MutArray.t = MutArray.create() in
	let add_part = MutArray.add parts in

	let any_so_far(): bool =
		not @@ MutArray.empty parts in

	let finish_regular(): Ast.expr =
		let loc = Lexer.loc_from l expr_start in
		match MutArray.length parts with
		| 0 ->
			ErrU.raise loc Err.EmptyExpression
		| 1 ->
			MutArray.get parts 0
		| _ ->
			let head = MutArray.get parts 0 in
			let tail = MutArray.tail parts in
			Ast.Call(loc, head, tail) in

	let dot_dot(left: Ast.expr): Ast.expr * next =
		let right_start, next = Lexer.pos_next l in
		match next with
		| Token.Colon ->
			raise U.TODO
		| _ ->
			let right, next = parse_expr_with_next l right_start next ctx in
			Ast.Partial(Lexer.loc_from l expr_start, left, [| right |]), next in

	let rec recur(start: Loc.pos)(next: Token.t) =
		let unexpected() = ParseU.unexpected start l next in
		match next with
		| Token.Cs ->
			ErrU.check (ctx = Line) (Lexer.loc_from l start) Err.CsMustBeInLineContext;
			let expr = parse_cs l start in
			add_part expr;
			finish_regular(), EndNestedBlock

		| Token.Colon ->
			let expr, next = parse_expr l ctx in
			add_part expr;
			finish_regular(), next

		| Token.Ck ->
			ErrU.check (not (any_so_far()) && ctx = Line) (Lexer.loc_from l start) Err.EqualsInExpression; (*TODO: Err.CkMustBeAtStart*)
			let cond, next = parse_expr l ExprOnly in
			Ast.Check(Lexer.loc_from l expr_start, cond), next

		| Token.Equals ->
			ErrU.check (ctx = Line) (Lexer.loc_from l start) Err.EqualsInExpression;
			let fail() = ErrU.raise (Lexer.loc_from l start) Err.PrecedingEquals in
			begin match MutArray.length parts with
			| 1 ->
				begin match MutArray.get parts 0 with
				| Ast.ExprAccess(loc, name) ->
					let declare = loc, name in
					let expr, next = parse_expr l ExprOnly in
					ErrU.check (next != CtxEnded) (Lexer.loc_from l start) Err.BlockCantEndInDeclare;
					assert (next = NewlineAfterStatement);
					expr, NewlineAfterEquals declare
				| _ ->
					fail()
				end
			| _ ->
				(*TODO: `p Point x y` and `Point x y`*)
				fail()
			end

		| Token.DotDot ->
			let left = finish_regular() in
			dot_dot left

		| Token.TypeName _ ->
			let typ = ParseType.f_with_start l start next in
			let start, next = Lexer.pos_next l in
			begin match next with
			| Token.At ->
				let expr, next = parse_expr l ctx in
				let loc = Lexer.loc_from l expr_start in
				Ast.At(loc, typ, expr), next
			| x ->
				add_part (Ast.ExprType typ);
				recur start x
			end

		(*TODO:KILL| Token.At ->
			let typ =
				match MutArray.length parts with
				| 0 ->
					let loc = Lexer.loc_from l expr_start in
					ErrU.raise loc Err.EmptyExpression (*TODO: more appropriate error: expected a type left of '@'*)
				| 1 ->
					MutArray.get parts 0
				| _ ->
					unexpected() in
			let expr, next = parse_expr l ctx in
			let loc = Lexer.loc_from l expr_start in
			Ast.At(loc, typ, expr), next*)

		| Token.Operator name ->
			let op = Ast.ExprAccess(Lexer.loc_from l start, name) in
			if any_so_far() then begin
				let left = finish_regular() in
				let right, next = parse_expr l ctx in
				Ast.Call(Lexer.loc_from l expr_start, op, [| left; right |]), next
			end else begin
				let right_start, next = Lexer.pos_next l in
				match ctx with
					| Line | Quote | CsHead -> unexpected()
					| ExprOnly ->
						begin match next with
						| Token.DotDot ->
							dot_dot op
						| Token.Newline | Token.Indent | Token.Dedent ->
							raise U.TODO
						| x -> ParseU.unexpected right_start l x
						end
					| Paren ->
						begin match next with
						| Token.Rparen ->
							(* (+) refers to the function itself. *)
							op, CtxEnded
						| Token.DotDot ->
							dot_dot op
						| x -> ParseU.unexpected right_start l x
						end
				end

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
			| _ ->
				OutputU.printf "%a\n" output_ctx ctx; (*TODO:KILL*)
				unexpected()
			end

		| Token.Indent ->
			begin match ctx with
			| CsHead ->
				finish_regular(), CtxEnded
			| Line ->
				let start, next = Lexer.pos_next l in
				let expr = parse_block l start next in
				add_part expr;
				finish_regular(), EndNestedBlock
			| _ ->
				unexpected()
			end

		| Token.QuoteStart s ->
			add_part (parse_quote l start s);
			let start, next = Lexer.pos_next l in
			recur start next

		| Token.RCurly ->
			finish_regular(), begin match ctx with
			| Quote -> CtxEnded
			| _ -> unexpected()
			end

		| x ->
			add_part (parse_single l start x);
			(*TODO: duplicate code below*)
			let start, next = Lexer.pos_next l in
			recur start next in

	recur expr_start next

and parse_quote(l: Lexer.t)(start: Loc.pos)(head: string): Ast.expr =
	let parts =
		ArrayU.build_loop begin fun () ->
			let interpolated, next = parse_expr l Quote in
			assert (next = CtxEnded);
			let s, is_done = Lexer.next_quote_part l in
			(interpolated, s), not is_done
		end in
	Ast.Quote(Lexer.loc_from l start, head, parts)

and parse_expr(l: Lexer.t)(ctx: ctx): Ast.expr * next =
	let start, next = Lexer.pos_next l in
	parse_expr_with_next l start next ctx

and parse_cs_parts(l: Lexer.t): Ast.cs_part array =
	ArrayU.build_until_none begin fun () ->
		let start, next = Lexer.pos_next l in
		match next with
		| Token.Dedent ->
			None
		| x ->
			let typ = ParseType.f_with_start l start x in
			begin match Lexer.next l with
			| Token.At ->
				let declare = ParseU.parse_name_with_loc l in
				let test = Ast.AtTest(Lexer.loc_from l start, typ, declare) in
				ParseU.must_skip l Token.Indent;
				let result = f l in
				Some(Lexer.loc_from l start, test, result)
			| x ->
				ParseU.unexpected start l x
			end
	end

and parse_cs(l: Lexer.t)(start: Loc.pos): Ast.expr =
	let cased, next = parse_expr l CsHead in
	assert (next = CtxEnded);
	Ast.Cs((Lexer.loc_from l start), cased, parse_cs_parts l)

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
