type pos = int
(* TODO: compact representation *)
type t = { start: pos; rear: pos }

let make(start: pos)(rear: pos): t =
	{ start; rear }

(*TODO: this is evil. Try being less evil in future*)
let single(start: pos): t =
	{ start; rear = start }

type lc_pos = { line: int; column: int }

type lc_loc = { lc_start: lc_pos; lc_rear: lc_pos }

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
	{ line = !line; column = !column }

let start_lc = { line = 0; column = 0 }

let lc_pos(source: BatIO.input)(pos: pos): lc_pos =
	walk_to source start_lc pos

let lc_loc(source: BatIO.input)(loc: t): lc_loc =
	let a = walk_to source start_lc loc.start in
	let b = walk_to source a (loc.rear - loc.start) in
	{ lc_start = a; lc_rear = b }


let output_pos(out: 'a OutputU.t)(pos: pos): unit =
	OutputU.out out "%d" pos

let output(out: 'a OutputU.t)({start; rear}: t): unit =
	OutputU.out out "%a-%a" output_pos start output_pos rear

let output_lc_pos(out: 'a OutputU.t)({line; column}: lc_pos): unit =
	OutputU.out out "%d:%d" line column

let output_lc_loc(out: 'a OutputU.t)({lc_start; lc_rear}: lc_loc): unit =
	OutputU.out out "%a-%a" output_lc_pos lc_start output_lc_pos lc_rear

(* type pos = { line: int; column: int }
type t = { start: pos; rear: pos }

let start_pos = { line = 1; column = 1 }

(*TODO: KILL?*)
(* let pos_column_shift p n =
	{ p with column = p.column + n } *)

let pos(line: int)(column: int): pos =
	{ line; column }
let line(pos: pos): int =
	pos.line
let column(pos: pos): int =
	pos.column


let make(start: pos)(rear: pos): t =
	{ start = start; rear = rear }

let single(p: pos): t =
	make p p

let next_line(p: pos): pos =
	{ line = p.line + 1; column = start_pos.column }

let prev_line(p: pos): pos =
	(*TODO: find prev column*)
	{ p with line = p.line - 1 }

let next_column(p: pos): pos =
	{ p with column = p.column - 1 }

(* boilerplate *)

let output_pos(out: 'a OutputU.t)({line; column}): unit =
	OutputU.out out "%d:%d" line column

let output(out: 'a OutputU.t)({start; rear}: t): unit =
	OutputU.out out "%a-%a" output_pos start output_pos rear
*)
