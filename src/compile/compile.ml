(*TODO: this is just for debugging, so move it to elsewhre.*)
let lex(io: FileIo.t)(path: Path.t): (Token.t * Loc.t) array =
	FileIoU.read io path begin fun source ->
		let l = Lexer.make source in
		ArrayU.build begin fun build ->
			let rec recur() =
				let start, next = Lexer.pos_next l in
				match next with
				| Token.EOF ->
					()
				| Token.QuoteStart _ ->
					build (next, Lexer.loc_from l start);
					lex_quote()
				| token ->
					build (token, Lexer.loc_from l start);
					recur()

			and lex_quote() =
				(*TODO:NEATER*)
				let rec foo() =
					let start, next = Lexer.pos_next l in
					match next with
					| Token.EOF ->
						true
					| Token.RCurly ->
						build (Token.RCurly, Lexer.loc_from l start);
						false
					| token ->
						build (token, Lexer.loc_from l start);
						foo() in
				let is_done = foo() in
				if is_done then
					()
				else begin
					let _, quote_done = Lexer.next_quote_part l in
					if quote_done then recur() else lex_quote()
				end in
			recur()
		end
	end

let check_and_generate(get_modul: Path.rel -> N.modul)(path: Path.t)(full_path: Path.t)(ast: Ast.modul): N.modul =
	let bindings = Bind.bind get_modul ast in
	let modul, type_of_ast = TypeOfAst.build path full_path bindings ast in
	let tys = TypeCheck.f bindings type_of_ast ast in
	CodeGen.f bindings type_of_ast tys ast;
	modul
