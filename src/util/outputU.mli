type 'a t
type ('a, 'o) printer = 'o t -> 'a -> unit

val printf: ('b, 'a t, unit) format -> 'b

val out: 'a t -> ('b, 'a t, unit) format -> 'b
val str: (string, 'o) printer

val out_array: ('a, 'o) printer -> ('a array, 'o) printer
val out_option: ('a, 'o) printer -> ('a option, 'o) printer
val out_pair: ('a, 'o) printer -> ('b, 'o) printer -> ('a * 'b, 'o) printer

(* 'a should always be string *)
val out_to_string: ('a, 'b t, unit, string) format4 -> 'a

val output_int: (int, 'o) printer
