let parse_signature(l: Lexer.t): Ast.signature * Token.t =
	let start = Lexer.pos l in
	let return = ParseTy.f l in
	let params, next =
		ArrayU.build_loop0 @@ fun () ->
			let start, next = Lexer.pos_next l in
			match next with
			| Token.Indent | Token.Newline | Token.Dedent ->
				ArrayU.Done(next)
			| Token.Name name ->
				let ty = ParseTy.f l in
				ArrayU.Cont (Lexer.loc_from l start, name, ty)
			| t ->
				ParseU.unexpected start l t in
	(Lexer.loc_from l start, return, params), next

let parse_fn(l: Lexer.t)(start: Loc.pos): Ast.fn =
	let name = ParseU.parse_name l in
	let signature, next = parse_signature l in
	ParseU.expect start l Token.Indent next;
	let value = ParseBlock.f l in
	Lexer.loc_from l start, name, signature, value

let parse_rt(l: Lexer.t)(start: Loc.pos): Ast.rt =
	let name = ParseU.parse_ty_name l in
	ParseU.must_skip l Token.Indent;
	let props = ArrayU.build_loop @@ fun () ->
		let prop =
			let start = Lexer.pos l in
			let name = ParseU.parse_name l in
			let ty = ParseTy.f l in
			Lexer.loc_from l start, name, ty in
		let start, next = Lexer.pos_next l in
		prop, match next with
		| Token.Dedent ->
			false
		| Token.Newline ->
			true
		| x ->
			ParseU.unexpected start l x in
	Lexer.loc_from l start, name, props

let parse_un(l: Lexer.t)(start: Loc.pos): Ast.un =
	let name = ParseU.parse_ty_name l in
	ParseU.must_skip l Token.Indent;
	let tys = ArrayU.build_loop @@ fun () ->
		let ty = ParseTy.f l in
		let start, next = Lexer.pos_next l in
		ty, match next with
		| Token.Dedent ->
			false
		| Token.Newline ->
			true
		| x ->
			ParseU.unexpected start l x in
	Lexer.loc_from l start, name, tys

let parse_ft(l: Lexer.t)(start: Loc.pos): Ast.ft =
	let name = ParseTy.parse_ty_name_or_generic l in
	let signature, next = parse_signature l in
	begin match next with
	| Token.Indent | Token.Newline ->
		ParseU.unexpected start l next
	| Token.Dedent ->
		()
	| _ ->
		assert false
	end;
	Lexer.loc_from l start, name, signature

let f(l: Lexer.t)(start: Loc.pos)(next: Token.t): Ast.decl =
	match next with
	| Token.Fn ->
		Ast.DeclVal(Ast.Fn(parse_fn l start))
	| Token.Rt ->
		Ast.DeclTy(Ast.Rt(parse_rt l start))
	| Token.Un ->
		Ast.DeclTy(Ast.Un(parse_un l start))
	| Token.Ft ->
		Ast.DeclTy(Ast.Ft(parse_ft l start))
	| x ->
		ParseU.unexpected start l x
