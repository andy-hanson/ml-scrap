(*TODO: this needs to be changed to deal with interpolation.*)
let lex(io: FileIO.t)(file_name: string)(ctx: CompileContext.t): (Token.t * Loc.t) array =
	io#read file_name begin fun source ->
		let l = Lexer.make (CompileContext.warn ctx) source in
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

let f(io: FileIO.t)(file_name: string)(ctx: CompileContext.t): N.modul =
	let modul_ast = io#read file_name (Parse.f ctx) in
	let bindings = Bind.bind modul_ast in
	let binding = Bind.binding bindings in
	let modul, type_of_ast = TypeOfAst.build file_name binding modul_ast in
	let types = TypeCheck.f binding type_of_ast modul_ast in
	CodeGen.f bindings type_of_ast types modul_ast;
	modul
