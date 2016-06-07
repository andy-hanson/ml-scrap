type t = {
	ctx: CompileContext.t;
	r: Reader.t;
	mutable indent: int;
	mutable dedenting: int;
}

let make(ctx: CompileContext.t)(source: BatIO.input): t = {
	ctx;
	r = Reader.make source;
	indent = 0;
	dedenting = 0
}

let pos(l: t): Loc.pos =
	Reader.pos l.r

let loc_from(l: t)(start: Loc.pos): Loc.t =
	Loc.make start (Reader.pos l.r)

let loc_at(l: t): Loc.t =
	Loc.single (pos l)

let lex_indent(l: t): int =
	let start = pos l in
	let indent = Reader.skip_tabs l.r in
	CompileError.check ((Reader.peek l.r) != ' ') (loc_from l start) CompileError.LeadingSpace;
	indent

let rec next(l: t): Token.t =
	if not (l.dedenting = 0) then begin
		l.dedenting <- l.dedenting - 1;
		Token.Dedent
	end else
	let ch = Reader.next l.r in
	match ch with
	| '\x00' ->
		Token.End

	| ' ' ->
		if (Reader.peek l.r = '\n') then
			CompileContext.warn l.ctx (Loc.single (pos l)) CompileError.TrailingSpace;
		next l

	| '\n' ->
		let old_indent = l.indent in
		l.indent <- lex_indent l;
		if l.indent > old_indent then begin
			CompileError.check (l.indent = old_indent + 1) (Loc.single (pos l)) CompileError.TooMuchIndent;
			Token.Indent
		end else if l.indent = old_indent then begin
			Reader.skip_newlines l.r; (*TODO: this only happens if l.indent = 0*)
			Token.Newline
		end else begin
			Reader.skip_newlines l.r;
			l.dedenting <- (old_indent - l.indent) - 1;
			Token.Dedent
		end

	| '|' ->
		Reader.skip_rest_of_line l.r;
		next l

	| ':' -> Token.Colon
	| '=' -> Token.Equals
	| '(' -> Token.Lparen
	| ')' -> Token.Rparen

	| '-' ->
		let next = Reader.next l.r in
		if next = ' ' then
			Token.Name (CompileContext.symbol l.ctx "-")
		else begin
			CompileError.check (CharU.is_digit next) (Loc.single (pos l)) CompileError.NegMustPrecedeNumber;
			Reader.take_num_decimal true next l.r
		end

	| '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
		Reader.take_num_decimal false ch l.r

	| 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'
	| 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
	| '+' | '*' | '/' | '^' | '?' | '<' | '>' ->
		let str = Reader.take_name_like ch l.r in
		let name = CompileContext.symbol l.ctx str in
		BatOption.((CompileContext.keyword l.ctx name) |? (Token.Name name))

	| 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'
	| 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' ->
		let name = CompileContext.symbol l.ctx (Reader.take_name_like ch l.r) in
		Token.TypeName name

	| ch ->
		CompileError.raise (Loc.single (pos l)) (CompileError.UnrecognizedCharacter ch)
