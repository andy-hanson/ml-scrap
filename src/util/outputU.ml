type 'o t = 'o BatIO.output

(*TODO: just use BatIO.printer (is backwards from this...)*)
type ('a, 'o) printer = ('a, 'o) BatIO.printer

let printf = Batteries.Printf.printf

let out = Batteries.Printf.fprintf

let str = BatIO.nwrite

let out_array(output: ('a, 'o) printer)(out: 'o t)(arr: 'a array): unit =
	str out "[| ";
	ArrayU.iter arr begin fun em ->
		output out em;
		str out "; "
	end;
	str out "|]"

let out_hashtbl(out_key: ('k, 'o) printer)(out_val: ('v, 'o) printer)(out: 'o t)(tbl: ('k, 'v) Hashtbl.t): unit =
	str out "{ ";
	HashU.iter tbl begin fun key value ->
		out_key out key;
		str out ": ";
		out_val out value;
		str out ", "
	end;
	str out "}"

let out_option(output: ('a, 'o) printer)(o: 'o t)(op: 'a option): unit =
	match op with
	| Some value ->
		out o "Some(%a)" output value
	| None ->
		str o "None"

let out_to_string = Batteries.Printf.sprintf2

let output_int(o: 'o t)(i: int): unit =
	out o "%d" i
