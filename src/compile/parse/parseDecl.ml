let parse_param(p: Parser.t): Ast.parameter option =
	let start = Parser.pos p in
	match Parser.next p with
	| Token.Indent ->
		None
	| Token.Name name ->
		 let typ = ParseType.f p in
		 Some (Ast.Parameter(Parser.loc_from p start, name, typ))
	| t ->
		ParseU.unexpected p t

let parse_signature(p: Parser.t): Ast.signature =
	let start = Parser.pos p in
	let return_type = ParseType.f p in
	let params = ArrayU.build_array (fun () -> parse_param p) in
	Ast.Signature(Parser.loc_from p start, return_type, params)

let parse_fn(p: Parser.t)(start: Loc.pos): Ast.decl =
	let name = ParseU.parse_name p in
	let signature = parse_signature p in
	let value = ParseExpr.parse_block p in
	Ast.Val (Ast.DeclVal(Parser.loc_from p start, name, Ast.Fn(signature, value)))

let parse_property(p: Parser.t): Ast.property =
	let start = Parser.pos p in
	let name = ParseU.parse_name p in
	let typ = ParseType.f p in
	Ast.Property(Parser.loc_from p start, name, typ)

let parse_properties(p: Parser.t): Ast.property array =
	ArrayU.build_array_2 begin fun () ->
		let prop = parse_property p in
		match Parser.next p with
		| Token.Dedent ->
			(prop, false)
		| Token.Newline ->
			(prop, true)
		| x ->
			ParseU.unexpected p x
	end

let parse_rec(p: Parser.t)(start: Loc.pos): Ast.decl =
	let name = ParseU.parse_type_name p in
	ParseU.must_skip p Token.Indent;
	let props = parse_properties p in
	Ast.Type (Ast.DeclType(Parser.loc_from p start, name, Ast.Rec(props)))


(* parse module declaration or End *)
let try_parse_decl(p: Parser.t): Ast.decl option =
	let start = Parser.pos p in
	match Parser.next p with
	| Token.End ->
		None
	| Token.Fn ->
		Some (parse_fn p start)
	| Token.Rec ->
		Some (parse_rec p start)
	| x ->
		ParseU.unexpected p x
