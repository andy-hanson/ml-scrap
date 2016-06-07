type pos = { line: int; column: int }
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
