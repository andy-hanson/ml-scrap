let parse_signature(l: Lexer.t): Ast.signature * Token.t =
	let start = Lexer.pos l in
	let return_type = ParseType.f l in
	(*TODO: way to not use ref?*)
	let next_token = ref Token.End in
	let params = ArrayU.build_until_none begin fun () ->
		let start, next = Lexer.pos_next l in
		match next with
		| Token.Indent | Token.Newline | Token.Dedent ->
			next_token := next;
			None
		| Token.Name name ->
			let typ = ParseType.f l in
			Some (Lexer.loc_from l start, name, typ)
		| t ->
			ParseU.unexpected start l t
	end in
	(Lexer.loc_from l start, return_type, params), !next_token

let parse_fn(l: Lexer.t)(start: Loc.pos): Ast.fn =
	let name = ParseU.parse_name l in
	let signature, next = parse_signature l in
	ParseU.expect start l Token.Indent next;
	let value = ParseBlock.f l in
	Lexer.loc_from l start, name, signature, value

let parse_cn(l: Lexer.t)(start: Loc.pos): Ast.cn =
	let name = ParseU.parse_name l in
	let typ = ParseType.f l in
	ParseU.must_skip l Token.Indent;
	let parts = ParseBlock.parse_case_parts l in
	Lexer.loc_from l start, name, typ, parts

let parse_rt(l: Lexer.t)(start: Loc.pos): Ast.rt =
	let name = ParseU.parse_type_name l in
	ParseU.must_skip l Token.Indent;
	let props = ArrayU.build_loop begin fun () ->
		let prop =
			let start = Lexer.pos l in
			let name = ParseU.parse_name l in
			let typ = ParseType.f l in
			Lexer.loc_from l start, name, typ in
		let start, next = Lexer.pos_next l in
		prop, match next with
		| Token.Dedent ->
			false
		| Token.Newline ->
			true
		| x ->
			ParseU.unexpected start l x
	end in
	Lexer.loc_from l start, name, props

let parse_un(l: Lexer.t)(start: Loc.pos): Ast.un =
	let name = ParseU.parse_type_name l in
	ParseU.must_skip l Token.Indent;
	let types = ArrayU.build_loop begin fun () ->
		let typ = ParseType.f l in
		let start, next = Lexer.pos_next l in
		typ, match next with
		| Token.Dedent ->
			false
		| Token.Newline ->
			true
		| x ->
			ParseU.unexpected start l x
	end in
	Lexer.loc_from l start, name, types

let parse_ft(l: Lexer.t)(start: Loc.pos): Ast.ft =
	let name = ParseU.parse_type_name l in
	ParseU.must_skip l Token.Indent;
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
	(*TDOO: parse_signature helper?*)

let parse_ct(l: Lexer.t)(start: Loc.pos): Ast.ct =
	(*TODO: most decls start this way, so I guess it's duplicate code*)
	let name = ParseU.parse_type_name l in
	ParseU.must_skip l Token.Indent;
	let cases = ArrayU.build_loop begin fun () ->
		let return = ParseType.f l in
		let input = ParseType.f l in
		let start, next = Lexer.pos_next l in
		(return, input), match next with
		| Token.Dedent ->
			false
		| Token.Newline ->
			true
		| x ->
			ParseU.unexpected start l x
	end in
	Lexer.loc_from l start, name, cases

(* parse module declaration or End *)
let try_parse_decl(l: Lexer.t): Ast.decl option =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.End ->
		None
	| Token.Fn ->
		Some(Ast.DeclFn(parse_fn l start))
	| Token.Cn ->
		Some (Ast.DeclCn(parse_cn l start))
	| Token.Rt ->
		Some(Ast.DeclRt(parse_rt l start))
	| Token.Un ->
		Some(Ast.DeclUn(parse_un l start))
	| Token.Ft ->
		Some(Ast.DeclFt(parse_ft l start))
	| Token.Ct ->
		Some(Ast.DeclCt(parse_ct l start))
	| x ->
		ParseU.unexpected start l x
