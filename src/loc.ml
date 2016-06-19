type pos = int
(* Storing Loc.t as int results in fewer heap allocations during lexing. *)
type t = int

(* 1 bit of each integer is reserved, so allow 30 bits per pos. So max_pos is 2^30. *)
let max_pos = 1024 * 1024 * 1024

(* Can't support 32 bit machines. *)
let () = assert ((max_pos * max_pos) / max_pos = max_pos)

let start(loc: t): pos =
	loc / max_pos
let rear(loc: t): pos =
	loc mod max_pos

let make(start: pos)(rear: pos): t =
	assert (start < max_pos);
	assert (rear < max_pos);
	start * max_pos + rear

let single_character(start: pos): t =
	make start (start + 1)

let hash(loc: t): int =
	loc

type lc_pos = {line: int; column: int}

type lc_loc = {lc_start: lc_pos; lc_rear: lc_pos}

let walk_to(source: BatIO.input)({line; column}: lc_pos)(distance_to_walk: int) =
	let line = ref line in
	let column = ref column in
	U.do_times distance_to_walk begin fun () ->
		(*Assuming pos is valid, this will not throw no_more_input*)
		match BatIO.read source with
		| '\n' ->
			incr line;
			column := 1
		| _ ->
			incr column
	end;
	{line = !line; column = !column}

let start_lc = {line = 1; column = 1}

let lc_pos(source: BatIO.input)(pos: pos): lc_pos =
	walk_to source start_lc pos

let lc_loc(source: BatIO.input)(loc: t): lc_loc =
	let a = walk_to source start_lc (start loc) in
	let b = walk_to source a ((rear loc) - (start loc)) in
	{lc_start = a; lc_rear = b}

let output_pos(out: 'o OutputU.t)(pos: pos): unit =
	OutputU.out out "%d" pos

let output(out: 'o OutputU.t)(loc: t): unit =
	OutputU.out out "%a-%a" output_pos (start loc) output_pos (rear loc)

let output_lc_pos(out: 'o OutputU.t)({line; column}: lc_pos): unit =
	OutputU.out out "%d:%d" line column

let output_lc_loc(out: 'o OutputU.t)({lc_start; lc_rear}: lc_loc): unit =
	OutputU.out out "%a-%a" output_lc_pos lc_start output_lc_pos lc_rear
