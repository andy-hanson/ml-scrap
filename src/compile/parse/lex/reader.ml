(*TODO: join with lexer?*)

let in_range(value: char)(min: char)(max: char): bool =
	min <= value && value <= max

type t = {
	source: string;
	mutable idx: int;
	(* TODO: rename to pos *)
	mutable p: Loc.pos
}

type restore = { idx: int; p: Loc.pos }

let get_restore(r: t): restore =
	{ idx = r.idx; p = r.p }

let do_restore(r: t)(restore: restore): unit =
	r.idx <- restore.idx;
	r.p <- restore.p



let pos(r: t): Loc.pos =
	r.p

let undo_read_newline(r: t): unit =
	r.idx <- r.idx - 1;
	(*TODO: get column by counting the line*)
	r.p <- { Loc.line = r.p.Loc.line - 1; Loc.column = 1 }

let can_peek(r: t)(n: int): bool =
	let peeked = r.idx + n in
	0 <= peeked && peeked < (String.length r.source)

let peek(r: t): char =
	String.get r.source r.idx

let peek_by(r: t)(n: int): char =
	String.get r.source (r.idx + n)

let skip(r: t): unit =
	r.idx <- r.idx + 1;
	r.p <- Loc.pos_column_shift r.p 1

let next(r: t): char =
	U.returning (peek r) (fun _ -> skip r)

let try_eat_if(r: t)(pred: char -> bool): bool =
	U.returning (pred (peek r)) (fun a -> if a then skip r)

let try_eat(r: t)(char_to_eat: char): bool =
	try_eat_if r (fun ch -> ch == char_to_eat)

let skip_rest_of_line(r: t): unit =
	raise U.TODO

let take_rest_of_line(r: t): string =
	raise U.TODO

let slice(r: t)(first: int)(last: int): string =
	BatString.slice ~first:first ~last:last r.source

let slice_from(r: t)(first: int): string =
	slice r first r.idx

let skip_while(r: t)(cond: char -> bool): unit =
	while cond (peek r) do
		skip r
	done

let take_num_decimal(r: t): Val.t =
	let start = r.idx - 1 in
	let is_digit ch = in_range ch '0' '9' in
	skip_while r is_digit;
	if (peek r) = '.' then begin
		if is_digit (peek_by r 1) then begin
			skip r;
			skip_while r is_digit
		end;
		let s = slice_from r start in
		Val.Float (float_of_string s)
	end else
		let s = slice_from r start in
		Val.Int (int_of_string s)

(*TODO: count is never used?*)
let count_newlines(r: t): int =
	let start_line = r.p.Loc.line in
	(*TODO: helper in Loc*)
	let incr_line () = r.p <- {r.p with Loc.line = r.p.Loc.line + 1} in
	incr_line ();
	while (peek r) = '\n' do
		r.idx <- r.idx + 1;
		incr_line ()
	done;
	(*TODO: helper in Loc*)
	r.p <- { r.p with Loc.column = Loc.start_pos.Loc.column };
	r.p.Loc.line - start_line

let take_name_like(r: t): string =
	let start = r.idx - 1 in
	(* TODO: Str.regex would speed this up a ton! *)
	skip_while r (fun ch -> in_range ch 'a' 'z');
	slice_from r start

let count_while(r: t)(cond: char -> bool): int =
	let count = ref 0 in
	while cond (peek r) do
		skip r;
		count := !count + 1
	done;
	!count

let skip_newlines(r: t): unit =
	ignore (count_newlines r)

let skip_tabs(r: t): int =
	count_while r (fun ch -> ch = '\t')



let make(source: string): t =
	let r = {
		source = source ^ "\x00";
		idx = 0;
		p = Loc.start_pos
	} in
	U.returning r skip_newlines
