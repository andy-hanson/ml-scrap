(*TODO: simplify*)
let f(l: Lexer.t): Ast.typ =
	let parse_type_with_next(start: Loc.pos)(token: Token.t): Ast.typ =
		match token with
		| Token.TypeName name ->
			Ast.TypeAccess(Ast.Access(Lexer.loc_from l start, name))
		(*| Token.Lparen ->
			let take_parts() = ArrayU.build_until_none begin fun () ->
				let start, next = Lexer.pos_next l in
				match next with
				| Token.Rparen ->
					None
				| x ->
					Some (parse_type_with_next start x)
			end in
			begin match Lexer.next l with
			| Token.FnType ->
				let parts = take_parts() in
				let loc = Lexer.loc_from l start in
				CompileErrorU.check (Array.length parts > 1) loc CompileError.FnNeedsParts;
				Ast.TypeFn(Lexer.loc_from l start, ArrayU.head parts, ArrayU.tail parts)
			| x ->
				ParseU.unexpected start l x
			end*)
		| x ->
			ParseU.unexpected start l x in

	let start, next = Lexer.pos_next l in
	parse_type_with_next start next
