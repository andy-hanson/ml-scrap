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

let f(io: FileIO.t)(file_name: string)(ctx: CompileContext.t): Val.modul =
	let Ast.Modul(decls) as modul = io#read file_name (Parse.f ctx) in
	let fn_asts, rc_asts, un_asts, ft_asts = AstU.modul_split decls in
	let bindings = Bind.bind decls in
	let binding = Bind.binding bindings in
	let type_of_ast = BuildTypeOfAst.f binding rc_asts un_asts ft_asts in
	let types = TypeCheck.f binding type_of_ast fn_asts in
	CodeGen.f file_name bindings type_of_ast types modul
