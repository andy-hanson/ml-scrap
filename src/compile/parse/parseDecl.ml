let parse_signature(l: Lexer.t): Ast.signature * Token.t =
	let start = Lexer.pos l in
	let return = ParseTy.inline l in
	let params, next =
		ArrayU.build_loop0 @@ fun () ->
			let start, next = Lexer.pos_next l in
			match next with
			| Token.Indent | Token.Newline | Token.Dedent ->
				ArrayU.Done(next)
			| Token.Name name ->
				let ty = ParseTy.inline l in
				ArrayU.Cont (Lexer.loc_from l start, name, ty)
			| t ->
				ParseU.unexpected start l t in
	(Lexer.loc_from l start, return, params), next

let parse_fn_head(l: Lexer.t): Ast.fn_head =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.Name n ->
		Ast.FnPlain n
	| Token.Lbracket ->
		let name = ParseU.parse_name l in
		let params =
			(*TODO: this is similar to code in parse_ty_or_generic *)
			ArrayU.build_until_none @@ fun () ->
				let start, next = Lexer.pos_next l in
				match next with
				| Token.TyName n -> Some(Lexer.loc_from l start, n)
				| Token.Rbracket -> None
				| t -> ParseU.unexpected start l t in
		assert (not @@ ArrayU.empty params); (*TODO: proper error*)
		Ast.FnGeneric(name, params)
	| t ->
		ParseU.unexpected start l t

let parse_fn(l: Lexer.t)(start: Loc.pos): Ast.fn =
	let head = parse_fn_head l in
	let signature, next = parse_signature l in
	ParseU.expect start l Token.Indent next;
	let value = ParseBlock.f l in
	Lexer.loc_from l start, head, signature, value

(*TODO: move to parse_ty?*)
(* Returns whether it ended on a newline (as opposed to a dedent *)
let parse_ty_then_newline_or_dedent(l: Lexer.t): Ast.ty * bool =
	let ty, next_start, next = ParseTy.free l in
	ty, match next with
	| Token.Newline -> true
	| Token.Dedent -> false
	| x -> ParseU.unexpected next_start l x

let parse_rt(l: Lexer.t)(start: Loc.pos): Ast.decl_ty =
	(*TODO: dont' call it name*)
	let name = ParseTy.parse_ty_name_or_generic l in
	let props = ArrayU.build_loop @@ fun () ->
		let start = Lexer.pos l in
		let name = ParseU.parse_name l in
		let ty, cont = parse_ty_then_newline_or_dedent l in
		let prop = Lexer.loc_from l start, name, ty in
		prop, cont in
	let loc = Lexer.loc_from l start in
	match name with
	| Ast.FnPlain name -> Ast.Rt(loc, name, props)
	| Ast.FnGeneric(name, params) -> Ast.GenRt(loc, name, params, props)

let parse_un(l: Lexer.t)(start: Loc.pos): Ast.decl_ty =
	let name = ParseTy.parse_ty_name_or_generic l in
	let tys = ArrayU.build_loop @@ fun () -> parse_ty_then_newline_or_dedent l in
	let loc = Lexer.loc_from l start in
	match name with
	| Ast.FnPlain name -> Ast.Un(loc, name, tys)
	| Ast.FnGeneric(name, params) -> Ast.GenUn(loc, name, params, tys)

let parse_ft(l: Lexer.t)(start: Loc.pos): Ast.decl_ty =
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
	let loc = Lexer.loc_from l start in
	match name with
	| Ast.FnPlain name -> Ast.Ft(loc, name, signature)
	| Ast.FnGeneric(name, params) -> Ast.GenFt(loc, name, params, signature)

let f(l: Lexer.t)(start: Loc.pos)(next: Token.t): Ast.decl =
	match next with
	| Token.Fn ->
		Ast.DeclVal(Ast.Fn(parse_fn l start))
	| Token.Rt ->
		Ast.DeclTy(parse_rt l start)
	| Token.Un ->
		Ast.DeclTy(parse_un l start)
	| Token.Ft ->
		Ast.DeclTy(parse_ft l start)
	| x ->
		ParseU.unexpected start l x
