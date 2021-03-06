type t = {
	source: BatIO.input;
	mutable peek: char;
	mutable pos: Loc.pos;
	mutable indent: int;
	(* Number of Token.Dedent we have to output before continuing to read *)
	mutable dedenting: int
}

let pos(l: t): Loc.pos =
	l.pos

let loc_from(l: t)(start: Loc.pos): Loc.t =
		Loc.make start l.pos

let safe_read(input: BatIO.input): char =
	try
		BatIO.read input
	with BatIO.No_more_input ->
		'\x00'

let read_char(l: t): char =
	U.returning l.peek @@ fun _ ->
		l.peek <- safe_read l.source;
		l.pos <- l.pos + 1

let skip(l: t): unit =
	ignore @@ read_char l

let skip_while({peek; source; _} as l: t)(pred: char -> bool): unit =
	if pred peek then begin
		l.peek <-
			try
				(* Returns the first char that's not skipped. *)
				U.loop0 @@ fun loop ->
					let ch = BatIO.read source in
					l.pos <- l.pos + 1;
					if pred ch then loop() else ch
			with BatIO.No_more_input ->
				'\x00'
	end

let buffer_while({peek; source; _} as l: t)(b: BatBuffer.t)(pred: char -> bool): unit =
	(*TODO: duplicate of above...*)
	if pred peek then begin
		BatBuffer.add_char b peek;
		(* Returns the first char that's not skipped. *)
		l.peek <-
			try
				U.loop0 @@ fun loop ->
					let ch = BatIO.read source in
					l.pos <- l.pos + 1;
					if pred ch then begin
						BatBuffer.add_char b ch;
						loop()
					end else
						ch
				with BatIO.No_more_input ->
					'\x00'
	end

let skip_newlines(l: t): unit =
	skip_while l @@ (=) '\n'

let make(source: BatIO.input): t =
	let l = {
		source;
		peek = safe_read source;
		pos = 0;
		indent = 0;
		dedenting = 0
	} in
	U.returning l skip_newlines

let next_quote_part(l: t): string * bool =
	(*TODO: BatBuffer.build helper*)
	let b = BatBuffer.create 16 in
	let is_done =
		U.loop0 @@ fun loop ->
			match read_char l with
			| '"' ->
				true
			| '{' ->
				false
			| '\n' ->
				U.todo() (*TODO: unterminated quote error*)
			| '\\' ->
				let ch = read_char l in
				BatBuffer.add_char b begin match ch with
				| '"' | '{' ->
					ch
				| 'n' ->
					'\n'
				| 't' ->
					'\t'
				| _ ->
					U.todo() (*TODO: bad escape error*)
				end;
				loop()
			| ch ->
				BatBuffer.add_char b ch;
				loop() in
	BatBuffer.contents b, is_done

(*TODO: inline*)
let take_string(l: t): Token.t =
	let str, is_done = next_quote_part l in
	if is_done then Token.Literal(Ast.String str) else Token.QuoteStart(str)

let rec next(l: t): Token.t =
	let loc_from = loc_from l in
	let read_char() = read_char l in
	let skip() = skip l in
	let skip_newlines() = skip_newlines l in
	let buffer_while = buffer_while l in

	let take_number(negate: bool)(fst: char): Token.t =
		let b = BatBuffer.create 4 in
		BatBuffer.add_char b fst;
		buffer_while b CharU.is_digit;
		let value =
			if (l.peek = '.') then begin
				skip();
				BatBuffer.add_char b '.';
				ErrU.check (CharU.is_digit l.peek) (Loc.single_character l.pos) Err.NumberMustHaveDigitsAfterDecimalPoint;
				buffer_while b CharU.is_digit;
				let s = BatBuffer.contents b in
				let f = float_of_string s in
				let f = if negate then -.f else f in
				Ast.Float f
			end else
				let s = BatBuffer.contents b in
				let i = int_of_string s in
				let i = if negate then -i else i in
				Ast.Int i in
		Token.Literal value in

	let take_symbol(fst: char)(pred: char -> bool)(make_token: Sym.t -> Token.t): Token.t =
		let b = BatBuffer.create 8 in
		BatBuffer.add_char b fst;
		buffer_while b pred;
		let name = Sym.of_buffer b in
		OpU.or_else (TokenU.keyword name) @@ fun () -> make_token name in

	let take_operator(ch: char): Token.t =
		take_symbol ch CharU.is_operator_char (fun s -> Token.Operator s) in

	let count_while(pred: char -> bool): int =
		let count = ref 0 in
		while pred l.peek do
			skip();
			count := !count + 1
		done;
		!count in

	let lex_indent(): int =
		let start = l.pos in
		U.returning (count_while ((=) '\t')) @@ fun _ ->
			ErrU.check (l.peek != ' ') (loc_from start) Err.LeadingSpace in

	let handle_newline(indent_only: bool): Token.t =
		(* We only want to output 1 newline, so skip all others. *)
		skip_newlines();
		let old_indent = l.indent in
		l.indent <- lex_indent();
		if l.indent > old_indent then begin
			ErrU.check (l.indent = old_indent + 1) (Loc.single_character l.pos) Err.TooMuchIndent;
			Token.Indent
		end else if l.indent = old_indent then begin
			if indent_only then
				next l
			else
				Token.Newline
		end else begin
			l.dedenting <- (old_indent - l.indent) - 1;
			Token.Dedent
		end in

	if not (l.dedenting = 0) then begin
		l.dedenting <- l.dedenting - 1;
		Token.Dedent
	end else
	let ch = read_char() in
	match ch with
	| '\x00' ->
		(* Remember to dedent before finishing *)
		if l.indent != 0 then begin
			l.indent <- l.indent - 1;
			Token.Dedent
		end else
			Token.EOF

	| ' ' ->
		ErrU.check (l.peek != '\n') (Loc.single_character l.pos) Err.TrailingSpace;
		next l

	| '\n' ->
		handle_newline false

	| '|' ->
		skip_while l ((!=) '\n');
		handle_newline true

	| ':' ->
		Token.Colon
	| '(' ->
		Token.Lparen
	| ')' ->
		Token.Rparen
	| '[' ->
		Token.Lbracket
	| ']' ->
		Token.Rbracket

	| '-' ->
		let next = read_char() in
		if CharU.is_digit next then
			take_number true next
		else
			take_operator ch

	| '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
		take_number false ch

	| 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'
	| 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' ->
		take_symbol ch CharU.is_name_char @@ fun s -> Token.Name s

	| 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'
	| 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' ->
		take_symbol ch CharU.is_name_char @@ fun s -> Token.TyName s

	| '@' | '+' | '*' | '/' | '^' | '?' | '<' | '>' | '=' ->
		take_operator ch

	| '.' ->
		begin match l.peek with
		| '.' ->
			skip();
			Token.DotDot
		| _ ->
			Token.Dot
		end

	| '"' ->
		take_string l

	| '}' ->
		Token.RCurly

	| ch ->
		ErrU.raise (Loc.single_character l.pos) @@ Err.UnrecognizedCharacter ch

let pos_next l =
	let p = pos l in
	p, next l
