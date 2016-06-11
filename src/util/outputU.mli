type 'a t
type ('a, 'o) printer = 'o t -> 'a -> unit

val printf: ('b, 'a t, unit) format -> 'b

val out: 'a t -> ('b, 'a t, unit) format -> 'b
val str: (string, 'o) printer

val out_array: ('a, 'o) printer -> 'o t -> 'a array -> unit
val out_hashtbl: ('a, 'o) printer -> ('b, 'o) printer -> 'o t -> ('a, 'b) Hashtbl.t -> unit
val out_option: ('a, 'o) printer -> 'o t -> 'a option -> unit

(* 'a should always be string *)
val out_to_string: ('a, 'b t, unit, string) format4 -> 'a

val output_int: (int, 'o) printer
