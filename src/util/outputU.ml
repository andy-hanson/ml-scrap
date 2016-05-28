type ('o, 'a) printer = 'o BatIO.output -> 'a -> unit

let out = Batteries.Printf.fprintf

let str = BatIO.nwrite

let out_array(output: ('o, 'a) printer)(out: 'o BatIO.output)(arr: 'a array): unit =
	BatIO.nwrite out "[| ";
	let o em =
		output out em;
		BatIO.nwrite out "; " in
	Array.iter o arr;
	BatIO.nwrite out "|]"

let out_hashtbl(out_key: ('o, 'a) printer)(out_val: ('o, 'b) printer)(out: 'o BatIO.output)(tbl: ('a, 'b) Hashtbl.t): unit =
	let output_pair key value =
		out_key out key;
		BatIO.nwrite out ": ";
		out_val out value;
		BatIO.nwrite out ", " in
	BatIO.nwrite out "{ ";
	Hashtbl.iter output_pair tbl;
	BatIO.nwrite out "}"

let out_to_string(p: ('o, 'a) printer)(value: 'a): string =
	let b = BatBuffer.create 0 in
	p (BatBuffer.output_buffer b) value;
	BatBuffer.contents b

let output_int(o: 'o BatIO.output)(i: int): unit =
	out o "%d" i
