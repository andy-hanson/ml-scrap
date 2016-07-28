type ctx =
	| Line
	| ExprOnly (* Like Line, but forbid an `=` because we're already in one *)
	| Paren (* Parse an expression and expect a ')' *)
	| Quote (* Look for a QuoteEnd *)
	| CsHead

(* For debugging...*)
(*let output_ctx(out: 'o OutputU.t)(ctx: ctx): unit =
	OutputU.str out begin match ctx with
	| Line -> "Line"
	| ExprOnly -> "ExprOnly"
	| Paren -> "Paren"
	| Quote -> "Quote"
	| CsHead -> "CsHead"
	end*)

type next =
	| NewlineAfterEquals of Ast.pattern
	| NewlineAfterStatement
	| EndNestedBlock
	| CtxEnded

let parts_to_pattern(loc: Loc.t)(parts: Ast.expr MutArray.t): Ast.pattern =
	let fail() = ErrU.raise loc Err.PrecedingEquals in
	let part_to_pattern(part: Ast.expr): Ast.pattern =
		match part with
		| Ast.ExprAccess access ->
			(* Accesses are structurally the same as declares *)
			let declare = access in
			Ast.PSingle declare
		| _ ->
			fail() in
	begin match MutArray.length parts with
	| 0 ->
		fail()
	| 1 ->
		part_to_pattern @@ MutArray.get parts 0
	| _ ->
		Ast.PDestruct(loc, MutArray.map_to_array parts part_to_pattern)
	end

(*TODO: neater*)
let rec parse_expr_with_next(l: Lexer.t)(expr_start: Loc.pos)(next: Token.t)(ctx: ctx): Ast.expr * next =
	let parts: Ast.expr MutArray.t = MutArray.create() in
	let add_part = MutArray.push parts in

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
			U.todo()
		| _ ->
			let right, next = parse_expr_with_next l right_start next ctx in
			Ast.Partial(Lexer.loc_from l expr_start, left, [| right |]), next in

	U.loop2 expr_start next @@ fun loop start next ->
		let unexpected() = ParseU.unexpected start l next in
		let read_and_loop() =
			let start, next = Lexer.pos_next l in
			loop start next in
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
			let loc = Lexer.loc_from l start in
			ErrU.check (ctx = Line) loc Err.EqualsInExpression;
			let pattern = parts_to_pattern loc parts in
			let expr, next = parse_expr l ExprOnly in
			ErrU.check (next != CtxEnded) (Lexer.loc_from l start) Err.BlockCantEndInDeclare;
			assert (next = NewlineAfterStatement);
			expr, NewlineAfterEquals pattern

		| Token.Dot ->
			let name = ParseU.parse_name l in
			let loc = Lexer.loc_from l start in
			ErrU.check (any_so_far()) loc Err.EqualsInExpression; (*TODO: Err.BeforeDot*)
			let prev = MutArray.pop parts in
			add_part (Ast.GetProperty(loc, prev, name));
			read_and_loop()

		| Token.DotDot ->
			let left = finish_regular() in
			dot_dot left

		| Token.TyName _ ->
			let ty = ParseTy.inline_with_start l start next in
			let start, next = Lexer.pos_next l in
			begin match next with
			| Token.At | Token.AtAt ->
				let kind = if next = Token.At then Ast.Convert else Ast.Exact in
				let expr, next = parse_expr l ctx in
				let loc = Lexer.loc_from l expr_start in
				Ast.At(loc, kind, ty, expr), next
			| x ->
				add_part (Ast.ExprType ty);
				loop start x
			end

		| Token.Lbracket ->
			let start2, next2 = Lexer.pos_next l in
			add_part begin match next2 with
			| Token.Name n ->
				let loc2 = Lexer.loc_from l start2 in
				let expr = Ast.ExprAccess(loc2, n) in
				let tys = ParseTy.parse_gen_inst l in
				let loc = Lexer.loc_from l start in
				Ast.GenInst(loc, expr, tys)
			| Token.TyName n ->
				let loc2 = Lexer.loc_from l start2 in
				let gen_ty = Ast.TyAccess(loc2, n) in
				let tys = ParseTy.parse_gen_inst l in
				let loc = Lexer.loc_from l start in
				Ast.ExprType(Ast.TyInst(loc, gen_ty, tys))
			| x ->
				ParseU.unexpected start2 l x
			end;
			read_and_loop()

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
							U.todo()
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
			read_and_loop()

		| Token.Rparen ->
			finish_regular(), begin match ctx with
			| Paren -> CtxEnded
			| _ -> unexpected()
			end

		| Token.Newline | Token.Dedent ->
			finish_regular(), begin match ctx with
			| Line | ExprOnly ->
				if next = Token.Newline then NewlineAfterStatement else CtxEnded
			| _ ->
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
			read_and_loop()

		| Token.RCurly ->
			finish_regular(), begin match ctx with
			| Quote -> CtxEnded
			| _ -> unexpected()
			end

		| t ->
			let loc = Lexer.loc_from l start in
			let e =
				match t with
				| Token.Name name (*| Token.TypeName name*) ->
					Ast.ExprAccess(loc, name)
				| Token.Literal value ->
					Ast.Literal(loc, value)
				| _ ->
					ParseU.unexpected start l t in
			add_part e;
			read_and_loop()

and parse_quote(l: Lexer.t)(start: Loc.pos)(head: string): Ast.expr =
	let parts =
		ArrayU.build_loop @@ fun () ->
			let interpolated, next = parse_expr l Quote in
			assert (next = CtxEnded);
			let s, is_done = Lexer.next_quote_part l in
			(interpolated, s), not is_done in
	Ast.Quote(Lexer.loc_from l start, head, parts)

and parse_expr(l: Lexer.t)(ctx: ctx): Ast.expr * next =
	let start, next = Lexer.pos_next l in
	parse_expr_with_next l start next ctx

and parse_cs_parts(l: Lexer.t): Ast.cs_part array =
	ArrayU.build_until_none @@ fun () ->
		let start, next = Lexer.pos_next l in
		match next with
		| Token.Dedent ->
			None
		| x ->
			let ty = ParseTy.inline_with_start l start x in
			let pattern =
				match Lexer.next l with
				| Token.AtAt ->
					let declare = ParseU.parse_name_with_loc l in
					let pattern = Ast.PSingle declare in
					ParseU.must_skip l Token.Indent;
					pattern
				(*TODO: Lparen to parse nested destructures...*)
				| Token.Name n ->
					let patterns =
						ArrayU.build_until_none_with_first (Ast.PSingle(Lexer.loc_from l start, n)) @@ fun () ->
							let start, next = Lexer.pos_next l in
							match next with
							| Token.Name n ->
								Some(Ast.PSingle(Lexer.loc_from l start, n))
							| Token.Indent ->
								None
							| x ->
								ParseU.unexpected start l x in
					Ast.PDestruct(Lexer.loc_from l start, patterns)
				| x ->
					ParseU.unexpected start l x
				in
			let test = Lexer.loc_from l start, ty, pattern in
			let result = f l in
			Some(Lexer.loc_from l start, test, result)

and parse_cs(l: Lexer.t)(start: Loc.pos): Ast.expr =
	let cased, next = parse_expr l CsHead in
	assert (next = CtxEnded);
	Ast.Cs((Lexer.loc_from l start), cased, parse_cs_parts l)

and parse_block(l: Lexer.t)(start: Loc.pos)(first: Token.t): Ast.expr =
	let expr, next = parse_expr_with_next l start first Line in
	begin match next with
	| NewlineAfterEquals pattern ->
		let rest = f l in
		let loc = Lexer.loc_from l start in
		Ast.Let(loc, pattern, expr, rest)
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
