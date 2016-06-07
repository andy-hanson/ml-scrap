let parse_param(l: Lexer.t): Ast.parameter option =
	let start = Lexer.pos l in
	match Lexer.next l with
	| Token.Indent ->
		None
	| Token.Name name ->
		 let typ = ParseType.f l in
		 Some (Ast.Parameter(Lexer.loc_from l start, name, typ))
	| t ->
		ParseU.unexpected l t

let parse_signature(l: Lexer.t): Ast.signature =
	let start = Lexer.pos l in
	let return_type = ParseType.f l in
	let params = ArrayU.build_array (fun () -> parse_param l) in
	Ast.Signature(Lexer.loc_from l start, return_type, params)

let parse_fn(l: Lexer.t)(start: Loc.pos): Ast.decl =
	let name = ParseU.parse_name l in
	let signature = parse_signature l in
	let value = ParseExpr.parse_block l in
	Ast.Val (Ast.DeclVal(Lexer.loc_from l start, name, Ast.Fn(signature, value)))

let parse_property(l: Lexer.t): Ast.property =
	let start = Lexer.pos l in
	let name = ParseU.parse_name l in
	let typ = ParseType.f l in
	Ast.Property(Lexer.loc_from l start, name, typ)

let parse_properties(l: Lexer.t): Ast.property array =
	ArrayU.build_array_2 begin fun () ->
		let prop = parse_property l in
		match Lexer.next l with
		| Token.Dedent ->
			(prop, false)
		| Token.Newline ->
			(prop, true)
		| x ->
			ParseU.unexpected l x
	end

let parse_rec(l: Lexer.t)(start: Loc.pos): Ast.decl =
	let name = ParseU.parse_type_name l in
	ParseU.must_skip l Token.Indent;
	let props = parse_properties l in
	Ast.Type (Ast.DeclType(Lexer.loc_from l start, name, Ast.Rec(props)))

(* parse module declaration or End *)
let try_parse_decl(l: Lexer.t): Ast.decl option =
	let start = Lexer.pos l in
	match Lexer.next l with
	| Token.End ->
		None
	| Token.Fn ->
		Some (parse_fn l start)
	| Token.Rec ->
		Some (parse_rec l start)
	| x ->
		ParseU.unexpected l x
