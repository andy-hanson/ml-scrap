let parse_signature(l: Lexer.t): Ast.signature * Token.t =
	let start = Lexer.pos l in
	let return_type = ParseType.f l in
	let params, next =
		ArrayU.build_and_return begin fun build ->
			let rec recur(): Token.t =
				let start, next = Lexer.pos_next l in
				match next with
				| Token.Indent | Token.Newline | Token.Dedent ->
					next
				| Token.Name name ->
					let typ = ParseType.f l in
					build (Lexer.loc_from l start, name, typ);
					recur()
				| t ->
					ParseU.unexpected start l t in
			recur()
		end in
	(Lexer.loc_from l start, return_type, params), next

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
	let parts = ParseBlock.parse_cs_parts l in
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

let parse_ct(l: Lexer.t)(start: Loc.pos): Ast.ct =
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
let parse_decl(l: Lexer.t)(start: Loc.pos)(next: Token.t): Ast.decl =
	match next with
	| Token.Fn ->
		Ast.Fn(parse_fn l start)
	| Token.Cn ->
		Ast.Cn(parse_cn l start)
	| Token.Rt ->
		Ast.Rt(parse_rt l start)
	| Token.Un ->
		Ast.Un(parse_un l start)
	| Token.Ft ->
		Ast.Ft(parse_ft l start)
	| Token.Ct ->
		Ast.Ct(parse_ct l start)
	| x ->
		ParseU.unexpected start l x
