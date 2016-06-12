let f(l: Lexer.t): Ast.typ =
	let rec parse_type_with_next(start: Loc.pos)(token: Token.t): Ast.typ =
		match token with
		| Token.TypeName name ->
			Ast.TypeAccess(Ast.Access(Lexer.loc_from l start, name))
		| Token.Lparen ->
			begin match Lexer.next l with
			| Token.Or ->
				let parts = ArrayU.build_until_none begin fun () ->
					let start, next = Lexer.pos_next l in
					match next with
					| Token.Rparen ->
						None
					| x ->
						Some (parse_type_with_next start x)
				end in
				Ast.Or(Lexer.loc_from l start, parts)
			| x ->
				ParseU.unexpected start l x
			end
		| x ->
			ParseU.unexpected start l x in

	let start, next = Lexer.pos_next l in
	parse_type_with_next start next
