type 'o t = 'o BatIO.output
type ('a, 'o) printer = 'o t -> 'a -> unit

val printf: ('b, 'a t, unit) format -> 'b
val dbg: 'a -> ('a, 'o) printer -> unit

val out: 'a t -> ('b, 'a t, unit) format -> 'b
val str: (string, 'o) printer

(*TODO: move to OpU*)
val out_option: ('a, 'o) printer -> ('a option, 'o) printer
val out_pair: ('a, 'o) printer -> ('b, 'o) printer -> ('a * 'b, 'o) printer

(* 'a should always be string *)
val out_to_string: ('a, 'b t, unit, string) format4 -> 'a

val output_int: (int, 'o) printer
val output_string_plain: (string, 'o) printer
val output_string_escaped: (string, 'o) printer
