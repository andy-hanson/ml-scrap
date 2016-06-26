let lex(io: FileIO.t)(file_name: string)(ctx: CompileContext.t): (Token.t * Loc.t) array =
	io#read file_name begin fun source ->
		let l = Lexer.make (CompileContext.warn ctx) source in
		ArrayU.build begin fun build ->
			let rec recur = fun () ->
				let start, next = Lexer.pos_next l in
				match next with
				| Token.End ->
					()
				| token ->
					build (token, Lexer.loc_from l start);
					recur() in
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
