type 'a t = 'a BatIO.output

(*TODO: just use BatIO.printer (is backwards from this...)*)
type ('o, 'a) printer = 'o t -> 'a -> unit

let printf = Batteries.Printf.printf

let out = Batteries.Printf.fprintf

let str = BatIO.nwrite

let out_array(output: ('o, 'a) printer)(out: 'o t)(arr: 'a array): unit =
	str out "[| ";
	ArrayU.iter arr begin fun em ->
		output out em;
		str out "; "
	end;
	str out "|]"

let out_hashtbl(out_key: ('o, 'a) printer)(out_val: ('o, 'b) printer)(out: 'o t)(tbl: ('a, 'b) Hashtbl.t): unit =
	let output_pair key value =
		out_key out key;
		str out ": ";
		out_val out value;
		str out ", " in
	str out "{ ";
	(*TODO:HashU.iter*)
	Hashtbl.iter output_pair tbl;
	str out "}"

let out_option(output: ('o, 'a) printer)(o: 'o t)(op: 'a option): unit =
	match op with
	| Some value ->
		out o "Some(%a)" output value
	| None ->
		str o "None"

let out_to_string(p: (string, 'a) printer)(value: 'a): string =
	let b = BatBuffer.create 0 in
	p (BatBuffer.output_buffer b) value;
	BatBuffer.contents b

let output_int(o: 'o t)(i: int): unit =
	out o "%d" i
