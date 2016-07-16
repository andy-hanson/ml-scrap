let rec parse_gen_inst(l: Lexer.t): Ast.ty array =
	ArrayU.build_until_none begin fun () ->
		let start, next = Lexer.pos_next l in
		match next with
		| Token.Rbracket -> None
		| x -> Some(f_with_start l start x)
	end

and f_with_start(l: Lexer.t)(start: Loc.pos)(next: Token.t): Ast.ty =
	match next with
	| Token.TyName name ->
		Ast.TyAccess(Lexer.loc_from l start, name)
	| Token.Lbracket ->
		let head = f l in
		let args = parse_gen_inst l in
		let loc = Lexer.loc_from l start in
		ErrU.check (not @@ ArrayU.empty args) loc Err.EmptyExpression; (*TODO: better error*)
		Ast.TyInst(loc, head, args)
	| x ->
		ParseU.unexpected start l x

and f(l: Lexer.t): Ast.ty =
	let start, next = Lexer.pos_next l in
	f_with_start l start next

let parse_ty_name_or_generic(l: Lexer.t): Ast.ty_name =
	let name = ParseU.parse_ty_name l in
	let params =
	ArrayU.build_until_none begin fun () ->
		let start, next = Lexer.pos_next l in
		match next with
		| Token.TyName n -> Some(Lexer.loc_from l start, n)
		| Token.Indent -> None
		| t -> ParseU.unexpected start l t
	end in
	if ArrayU.empty params then Ast.Plain name else Ast.Generic(name, params)
