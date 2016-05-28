type t = {
	r: Reader.t;
	mutable indent: int;
}

let make(source: string): t = {
	r = Reader.make source;
	indent = 0
}

type restore = { r: Reader.restore; indent: int }
let get_restore(l: t): restore =
	{ r = Reader.get_restore l.r; indent = l.indent }
let do_restore(l: t)(r: restore): unit =
	Reader.do_restore l.r r.r;
	l.indent <- r.indent

let pos(l: t): Loc.pos =
	Reader.pos l.r

let loc_from(l: t)(start: Loc.pos): Loc.t =
	Loc.make start (Reader.pos l.r)

let lex_indent(l: t): int =
	let start = pos l in
	let indent = Reader.skip_tabs l.r in
	CompileError.check ((Reader.peek l.r) != ' ') (loc_from l start) CompileError.LeadingSpace;
	indent

let lex_number(l: t): Token.t =
	let num = (Reader.take_num_decimal l.r) in
	Token.Literal num

let lex_name(l: t)(ctx: CompileContext.t): Token.t =
	let str = (Reader.take_name_like l.r) in
	let name = CompileContext.symbol ctx str in
	BatOption.((CompileContext.keyword ctx name) |? (Token.Name name))

let lex_type_name(l: t)(ctx: CompileContext.t): Token.t =
	let name = CompileContext.symbol ctx (Reader.take_name_like l.r) in
	Token.TypeName name

let rec next(l: t)(ctx: CompileContext.t): Token.t =
	match Reader.next l.r with
	| '\x00' ->
		Token.End

	| ' ' ->
		next l ctx

	| '\n' ->
		if ((Reader.can_peek l.r) (-2)) && ((Reader.peek_by l.r (-2)) = '.') then
			CompileContext.warn ctx (Loc.single (pos l)) CompileError.TrailingSpace;
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
		if '0' < (Reader.peek l.r) && (Reader.peek l.r) <= '9' then
			lex_number l
		else
			lex_name l ctx

	| '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
		lex_number l

	| 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'
	| 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
	| '+' | '*' | '/' | '^' | '?' | '<' | '>' ->
		lex_name l ctx

	| 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'
	| 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' ->
		lex_type_name l ctx

	| ch ->
		CompileError.raise (Loc.single (pos l)) (CompileError.UnrecognizedCharacter ch)
