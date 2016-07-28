type ty_or_non =
	| Ty of Ast.ty
	| NonTy of Token.t

let rec parse_part(l: Lexer.t)(start: Loc.pos)(next: Token.t): ty_or_non =
	match next with
	| Token.TyName name ->
		Ty(Ast.TyAccess(Lexer.loc_from l start, name))
	| Token.Lbracket ->
		let head = inline l in
		let args = parse_gen_inst l in
		let loc = Lexer.loc_from l start in
		ErrU.check (not @@ ArrayU.empty args) loc Err.EmptyExpression; (*TODO: better error*)
		Ty(Ast.TyInst(loc, head, args))
	| x ->
		NonTy(x)

and parse_gen_inst(l: Lexer.t): Ast.ty array =
	ArrayU.build_until_none @@ fun () ->
		let start, next = Lexer.pos_next l in
		match next with
		| Token.Rbracket -> None
		| x -> Some(inline_with_start l start x)

and inline_with_start(l: Lexer.t)(start: Loc.pos)(next: Token.t): Ast.ty =
	match parse_part l start next with
	| Ty t -> t
	| NonTy x -> ParseU.unexpected start l x

and inline(l: Lexer.t): Ast.ty =
	let start, next = Lexer.pos_next l in
	inline_with_start l start next

let free(l: Lexer.t): Ast.ty * Loc.pos * Token.t =
	let start = Lexer.pos l in
	let parts, (next_start, next) =
		ArrayU.build_and_return @@ fun build ->
			U.loop0 @@ fun loop ->
				let start, next = Lexer.pos_next l in
				match parse_part l start next with
				| Ty t ->
					build t;
					loop()
				| NonTy x ->
					start, x in
	let ty =
		if Array.length parts = 0 then
			ParseU.unexpected next_start l next
		else if Array.length parts = 1 then
			parts.(0)
		else
			let loc = Lexer.loc_from l start in
			Ast.TyInst(loc, parts.(0), ArrayU.tail parts) in
	ty, next_start, next

let parse_ty_name_or_generic(l: Lexer.t): Ast.fn_head =
	let name = ParseU.parse_ty_name l in
	let params =
		ArrayU.build_until_none @@ fun () ->
			let start, next = Lexer.pos_next l in
			match next with
			| Token.TyName n ->
				Some(Lexer.loc_from l start, n)
			| t ->
				if t = Token.Indent then None else ParseU.unexpected start l t in
	if ArrayU.empty params then Ast.FnPlain name else Ast.FnGeneric(name, params)
