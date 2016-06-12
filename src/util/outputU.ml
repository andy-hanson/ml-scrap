type 'o t = 'o BatIO.output

type ('a, 'o) printer = ('a, 'o) BatIO.printer

let printf = Batteries.Printf.printf

let out = Batteries.Printf.fprintf

let str = BatIO.nwrite

let out_array(output: ('a, 'o) printer)(out: 'o t)(arr: 'a array): unit =
	str out "[| ";
	ArrayU.iteri arr begin fun idx em ->
		output out em;
		if idx != Array.length arr - 1 then
			str out "; "
	end;
	str out " |]"

let out_option(output: ('a, 'o) printer)(o: 'o t)(op: 'a option): unit =
	match op with
	| Some value ->
		out o "Some(%a)" output value
	| None ->
		str o "None"

let out_pair(output_a: ('a, 'o) printer)(output_b: ('b, 'o) printer)(o: 'o t)((a, b): 'a * 'b): unit =
	out o "(%a, %a)" output_a a output_b b

let out_to_string = Batteries.Printf.sprintf2

let output_int(o: 'o t)(i: int): unit =
	out o "%d" i
