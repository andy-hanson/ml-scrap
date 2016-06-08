(*TODO: join with lexer?*)

type t = {
	source: BatIO.input;
	mutable peek: char; (* '\x00' if no more input *)
	mutable pos: Loc.pos
}

(*TODO: declaration order in this file could use cleanup*)

let safe_read(input: BatIO.input): char =
	try
		BatIO.read input
	with BatIO.No_more_input ->
		'\x00' (*TODO: give this constant a name*)

(*Advance without affecting pos*)
let advance(r: t): char =
	U.returning r.peek (fun _ -> r.peek <- safe_read r.source)

let pos(r: t): Loc.pos =
	r.pos

let peek(r: t): char =
	r.peek

let next(r: t): char =
	U.returning (advance r) begin fun _ ->
		r.pos <- r.pos + 1
		(* If it was a newline, lexer should tell us to skip_newlines, so pos will be set then *)
	end

let skip(r: t): unit =
	ignore (next r);
	r.pos <- r.pos + 1

let try_eat_if(r: t)(pred: char -> bool): bool =
	U.returning (pred (peek r)) (fun a -> if a then skip r)

let try_eat(r: t)(char_to_eat: char): bool =
	try_eat_if r (fun ch -> ch == char_to_eat)

(* let skip_while(r: t)(cond: char -> bool): unit =
	while cond (peek r) do
		skip r
	done *)

let buffer_while(b: BatBuffer.t)(r: t)(cond: char -> bool): unit =
	while cond (peek r) do
		BatBuffer.add_char b (next r)
	done

let take_num_decimal(negate: bool)(fst: char)(r: t): Token.t =
	let b = BatBuffer.create 4 in
	BatBuffer.add_char b fst;
	buffer_while b r CharU.is_digit;
	let value = if (peek r = '.') then begin
		raise U.TODO
		(*
		if is_digit (peek_by r 1) then begin
			skip r;
			skip_while r is_digit
		end;
		let s = slice_from r start in
		Val.Float (float_of_string s)
		*)
	end else
		let s = BatBuffer.contents b in
		let i = int_of_string s in
		let i = if negate then -i else i in
		Val.Int i in
	Token.Literal value

let take_name_like(fst: char)(r: t): string =
	(*TODO:build pattern*)
	let b = BatBuffer.create 4 in
	BatBuffer.add_char b fst;
	(*TODO: alter parameter order of in_range so we can curry here*)
	buffer_while b r CharU.is_name_char;
	BatBuffer.contents b

let count_while(r: t)(cond: char -> bool): int =
	let count = ref 0 in
	while cond (peek r) do
		skip r;
		count := !count + 1
	done;
	!count

let skip_rest_of_line(_: t): unit =
	raise U.TODO

let take_rest_of_line(_: t): string =
	raise U.TODO

let skip_newlines(r: t): unit =
	while (peek r) = '\n' do
		skip r
	done

let skip_tabs(r: t): int =
	count_while r (fun ch -> ch = '\t')

let make(source: BatIO.input): t =
	let r = {
		source;
		peek = safe_read source;
		pos = 0
	} in
	U.returning r skip_newlines
