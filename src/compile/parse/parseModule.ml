let parse_imports(_: Lexer.t): unit =
	raise U.TODO

let f(l: Lexer.t): Ast.modul =
	let start, next = Lexer.pos_next l in
	let imports, (start, next) =
		match next with
		| Token.Import ->
			let imports = parse_imports l in
			imports, Lexer.pos_next l
		| _ ->
			(), (start, next) in

	let decls = ArrayU.build_fold (start, next) begin fun (start, next) ->
		match next with
		| Token.EOF ->
			None, None
		| next ->
			let decl = ParseDecl.parse_decl l start next in
			Some(decl), Some(Lexer.pos_next l)
	end in

	imports, decls
