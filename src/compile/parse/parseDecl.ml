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
			Some (Ast.Parameter(Lexer.loc_from l start, name, typ))
		| t ->
			ParseU.unexpected start l t
	end in
	Ast.Signature(Lexer.loc_from l start, return_type, params), !next_token

let parse_fn(l: Lexer.t)(start: Loc.pos): Ast.decl =
	let name = ParseU.parse_name l in
	let signature, next = parse_signature l in
	ParseU.expect start l Token.Indent next;
	let value = ParseBlock.f l in
	Ast.DeclFn(Ast.Fn(Lexer.loc_from l start, name, signature, value))

let parse_rec(l: Lexer.t)(start: Loc.pos): Ast.decl =
	let name = ParseU.parse_type_name l in
	ParseU.must_skip l Token.Indent;
	let props = ArrayU.build_loop begin fun () ->
		let prop =
			let start = Lexer.pos l in
			let name = ParseU.parse_name l in
			let typ = ParseType.f l in
			Ast.Property(Lexer.loc_from l start, name, typ) in
		let start, next = Lexer.pos_next l in
		prop, match next with
		| Token.Dedent ->
			false
		| Token.Newline ->
			true
		| x ->
			ParseU.unexpected start l x
	end in
	Ast.DeclRc(Ast.Rc(Lexer.loc_from l start, name, props))

let parse_un(l: Lexer.t)(start: Loc.pos): Ast.decl =
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
	Ast.DeclUn(Ast.Un(Lexer.loc_from l start, name, types))

let parse_ft(l: Lexer.t)(start: Loc.pos): Ast.decl =
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
	Ast.DeclFt(Ast.Ft(Lexer.loc_from l start, name, signature))
	(*TDOO: parse_signature helper?*)

(* parse module declaration or End *)
let try_parse_decl(l: Lexer.t): Ast.decl option =
	let start, next = Lexer.pos_next l in
	match next with
	| Token.End ->
		None
	| Token.Fn ->
		Some (parse_fn l start)
	| Token.Rc ->
		Some (parse_rec l start)
	| Token.Un ->
		Some (parse_un l start)
	| Token.Ft ->
		Some (parse_ft l start)
	| x ->
		ParseU.unexpected start l x
