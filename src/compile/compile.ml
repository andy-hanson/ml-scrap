open N.Compiler

(*TODO: this is just for debugging, so move it to elsewhre.*)
let lex(source: BatIO.input): (Token.t * Loc.t) array =
	let l = Lexer.make source in
	ArrayU.build @@ fun build ->
		let rec lex_plain() =
			let start, next = Lexer.pos_next l in
			match next with
			| Token.EOF ->
				()
			| Token.QuoteStart _ ->
				build (next, Lexer.loc_from l start);
				lex_quote()
			| token ->
				build (token, Lexer.loc_from l start);
				lex_plain()

		and lex_quote() =
			let is_done =
				U.loop0 @@ fun loop ->
					(*TODO: lexer.loc_next*)
					let start, next = Lexer.pos_next l in
					let loc = Lexer.loc_from l start in
					match next with
					| Token.EOF ->
						true
					| Token.RCurly ->
						build (Token.RCurly, loc);
						false
					| token ->
						build (token, loc);
						loop() in
			if is_done then
				()
			else begin
				let _, quote_done = Lexer.next_quote_part l in
				if quote_done then lex_plain() else lex_quote()
			end in

		lex_plain()

let check_and_generate(get_modul: Path.rel -> modul)(path: Path.t)(full_path: Path.t)(ast: Ast.modul): modul =
	let bindings = Bind.bind get_modul ast in
	let modul, type_of_ast = TypeOfAst.build path full_path bindings ast in
	let tys = TypeCheck.f bindings type_of_ast ast in
	CodeGen.f bindings type_of_ast tys ast;
	modul
