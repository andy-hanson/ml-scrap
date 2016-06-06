type 'a t
type ('o, 'a) printer = 'o t -> 'a -> unit

(*TODO: don't reference batteries here, be abstract*)

val printf: ('b, 'a t, unit) BatPrintf.t -> 'b

val out: 'a t -> ('b, 'a t, unit) BatPrintf.t -> 'b
val str: ('a, string) printer

val out_array: ('o, 'a) printer -> 'o t -> 'a array -> unit
val out_hashtbl: ('o, 'a) printer -> ('o, 'b) printer -> 'o t -> ('a, 'b) Hashtbl.t -> unit
val out_option: ('o, 'a) printer -> 'o t -> 'a option -> unit

val out_to_string: (string, 'a) printer -> 'a -> string

val output_int: ('o, int) printer
