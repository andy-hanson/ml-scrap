type 'o t = 'o BatIO.output

type ('a, 'o) printer = ('a, 'o) BatIO.printer

let printf = Batteries.Printf.printf

let dbg(value: 'a)(printer: ('a, 'o) printer): unit =
	printf "\n!!! %a\n" printer value

let out = Batteries.Printf.fprintf

let str = BatIO.nwrite

let out_array_elements ?(delimeter=", ")(output: ('a, 'o) printer)(out: 'o t)(arr: 'a array): unit =
	ArrayU.iteri arr begin fun idx em ->
		output out em;
		if idx != Array.length arr - 1 then
			str out delimeter
	end

(*TODO: ArrayU*)
let out_array(output: ('a, 'o) printer)(out_channel: 'o t)(arr: 'a array): unit =
	out out_channel "[%a]" (out_array_elements output) arr

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

let output_string_escaped(o: 'o t)(s: string): unit =
	out o "\"%s\"" @@ String.escaped s
