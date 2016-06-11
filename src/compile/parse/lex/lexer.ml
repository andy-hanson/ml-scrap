type t = {
	ctx: CompileContext.t;
	source: BatIO.input;
	mutable peek: char; (* '\x00' if no more input *)
	mutable pos: Loc.pos;
	mutable indent: int;
	mutable dedenting: int;
}

let pos(l: t): Loc.pos =
	l.pos

let loc_from(l: t)(start: Loc.pos): Loc.t =
		Loc.make start l.pos

let safe_read(input: BatIO.input): char =
	try
		BatIO.read input
	with BatIO.No_more_input ->
		'\x00' (*TODO: give this constant a name*)

let read_char(l: t): char =
	U.returning l.peek begin fun _ ->
		l.peek <- safe_read l.source;
		l.pos <- l.pos + 1
	end

let skip(l: t): unit =
	ignore (read_char l)

let skip_while(l: t)(pred: char -> bool): unit =
	while pred l.peek do
		skip l
	done

let skip_newlines(l: t): unit =
	skip_while l ((=) '\n')

let make(ctx: CompileContext.t)(source: BatIO.input): t =
	let l = {
		ctx;

		source;
		peek = safe_read source;
		pos = 0;

		indent = 0;
		dedenting = 0
	} in
	U.returning l skip_newlines

let rec next(l: t): Token.t =
	let loc_from = loc_from l in
	let read_char() = read_char l in
	let skip() = skip l in
	let skip_while = skip_while l in
	let skip_newlines() = skip_newlines l in

	let buffer_while(b: BatBuffer.t)(cond: char -> bool): unit =
		while cond l.peek do
			BatBuffer.add_char b (read_char())
		done in

	let take_number(negate: bool)(fst: char): Token.t =
		let b = BatBuffer.create 4 in
		BatBuffer.add_char b fst;
		buffer_while b CharU.is_digit;
		let value =
			if (l.peek = '.') then begin
				skip();
				BatBuffer.add_char b '.';
				CompileError.check (CharU.is_digit l.peek) (Loc.single_character l.pos) CompileError.NumberMustHaveDigitsAfterDecimalPoint;
				buffer_while b CharU.is_digit;
				let s = BatBuffer.contents b in
				let f = float_of_string s in
				let f = if negate then -.f else f in
				Val.Float f
			end else
				let s = BatBuffer.contents b in
				let i = int_of_string s in
				let i = if negate then -i else i in
				Val.Int i in
		Token.Literal value in

	let take_name_like(fst: char): string =
		(*TODO:build pattern*)
		let b = BatBuffer.create 4 in
		BatBuffer.add_char b fst;
		(*TODO: alter parameter order of in_range so we can curry here*)
		buffer_while b CharU.is_name_char;
		BatBuffer.contents b in

	let count_while(pred: char -> bool): int =
		let count = ref 0 in
		while pred l.peek do
			skip();
			count := !count + 1
		done;
		!count in

	let count_tabs(): int = (*TODO:INLINE*)
		count_while ((=) '\t') in

	let lex_indent(): int =
		let start = l.pos in
		U.returning (count_tabs()) begin fun _ ->
			CompileError.check (l.peek != ' ') (loc_from start) CompileError.LeadingSpace
		end in

	let skip_rest_of_line(): unit =
		skip_while ((!=) '\n') in





	if not (l.dedenting = 0) then begin
		l.dedenting <- l.dedenting - 1;
		Token.Dedent
	end else
	let ch = read_char() in
	match ch with
	| '\x00' ->
		Token.End

	| ' ' ->
		if l.peek = '\n' then
			CompileContext.warn l.ctx (Loc.single_character l.pos) CompileError.TrailingSpace;
		next l

	| '\n' ->
		let old_indent = l.indent in
		l.indent <- lex_indent();
		if l.indent > old_indent then begin
			CompileError.check (l.indent = old_indent + 1) (Loc.single_character l.pos) CompileError.TooMuchIndent;
			Token.Indent
		end else if l.indent = old_indent then begin
			(* We only want to output 1 newline, so skip all others. *)
			skip_newlines(); (*TODO: this only happens if l.indent = 0*)
			Token.Newline
		end else begin
			skip_newlines();
			l.dedenting <- (old_indent - l.indent) - 1;
			Token.Dedent
		end

	| '|' ->
		skip_rest_of_line();
		next l

	| ':' -> Token.Colon
	| '=' -> Token.Equals
	| '(' -> Token.Lparen
	| ')' -> Token.Rparen

	| '-' ->
		let next = read_char() in
		if next = ' ' then
			Token.Name (CompileContext.symbol l.ctx "-")
		else begin
			CompileError.check (CharU.is_digit next) (Loc.single_character l.pos) CompileError.NegMustPrecedeNumber;
			take_number true next
		end

	| '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
		take_number false ch

	| 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'
	| 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
	| '+' | '*' | '/' | '^' | '?' | '<' | '>' ->
		let str = take_name_like ch in
		let name = CompileContext.symbol l.ctx str in
		BatOption.((CompileContext.keyword l.ctx name) |? (Token.Name name))

	| 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'
	| 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' ->
		let name = CompileContext.symbol l.ctx (take_name_like ch) in
		Token.TypeName name

	| ch ->
		(*TODO: Loc.single_character is called around here a lot...*)
		CompileError.raise (Loc.single_character l.pos) (CompileError.UnrecognizedCharacter ch)
