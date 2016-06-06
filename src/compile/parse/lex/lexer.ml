type t = {
	r: Reader.t;
	mutable indent: int;
}

let make(source: BatIO.input): t = {
	r = Reader.make source;
	indent = 0
}

let pos(l: t): Loc.pos =
	Reader.pos l.r

let loc_from(l: t)(start: Loc.pos): Loc.t =
	Loc.make start (Reader.pos l.r)

let lex_indent(l: t): int =
	let start = pos l in
	let indent = Reader.skip_tabs l.r in
	CompileError.check ((Reader.peek l.r) != ' ') (loc_from l start) CompileError.LeadingSpace;
	indent

let rec next(l: t)(ctx: CompileContext.t): Token.t =
	let ch = Reader.next l.r in
	match ch with
	| '\x00' ->
		Token.End

	| ' ' ->
		if (Reader.peek l.r = '\n') then
			CompileContext.warn ctx (Loc.single (pos l)) CompileError.TrailingSpace;
		next l ctx

	| '\n' ->
		let old_indent = l.indent in
		l.indent <- lex_indent l;
		if l.indent > old_indent then begin
			CompileError.check (l.indent = old_indent + 1) (Loc.single (pos l)) CompileError.TooMuchIndent;
			Token.Indent
		end else if l.indent = old_indent then begin
			Reader.skip_newlines l.r;
			Token.Newline
		end else if l.indent = old_indent - 1 then begin
			Reader.skip_newlines l.r;
			Token.Dedent
		end else begin
			(*
			Multiple dedents!
			Output one dedent and back up to before the newline.
			*)
			l.indent <- l.indent - 1;
			Reader.undo_read_newline l.r;
			Token.Dedent
		end

	| '|' ->
		Reader.skip_rest_of_line l.r;
		next l ctx

	| ':' -> Token.Colon
	| '=' -> Token.Equals
	| '(' -> Token.Lparen
	| ')' -> Token.Rparen

	| '-' ->
		let next = Reader.next l.r in
		if next = ' ' then
			Token.Name (CompileContext.symbol ctx "-")
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
		let name = CompileContext.symbol ctx str in
		BatOption.((CompileContext.keyword ctx name) |? (Token.Name name))

	| 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'
	| 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' ->
		let name = CompileContext.symbol ctx (Reader.take_name_like ch l.r) in
		Token.TypeName name

	| ch ->
		CompileError.raise (Loc.single (pos l)) (CompileError.UnrecognizedCharacter ch)
