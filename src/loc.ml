type pos = { line: int; column: int }
type t = { start: pos; rear: pos }

let start_pos = { line = 1; column = 1 }

let output_pos(out: 'a BatIO.output)({line; column}): unit =
	OutputU.out out "%d:%d" line column

let output(out: 'a BatIO.output)({start; rear}: t): unit =
	OutputU.out out "%a-%a" output_pos start output_pos rear

(* let make_pos line column =
	{ line = line; column = column }
 *)
let pos_column_shift p n =
	{ line = p.line; column = p.column + n }

let make start rear =
	{ start = start; rear = rear }

let single p =
	{ start = p; rear = p }
