val empty: 'a array -> bool

val same_length: 'a array -> 'b array -> bool

val iter: 'a array -> ('a -> unit) -> unit
val iteri: 'a array -> (int -> 'a -> unit) -> unit
val iter_zip: 'a array -> 'b array -> ('a -> 'b -> unit) -> unit

val map: 'a array -> ('a -> 'b) -> 'b array
val mapi: 'a array -> (int -> 'a -> 'b) -> 'b array
val map_zip: 'a array -> 'b array -> ('a -> 'b -> 'c) -> 'c array
val map_to_list: 'a array -> ('a -> 'b) -> 'b list
val zip: 'a array -> 'b array -> ('a * 'b) array

val fold_map: 'b -> 'a array -> ('b -> 'a -> 'b * 'c) -> 'b * 'c array

val filter: 'a array -> ('a -> bool) -> 'a array
val filter_map: 'a array -> ('a -> 'b option) -> 'b array

val find: 'a array -> ('a -> bool) -> 'a option
val find_index: 'a array -> ('a -> bool) -> int option
val find_map: 'a array -> ('a -> 'b option) -> 'b option
val exists: 'a array -> ('a -> bool) -> bool

val fold: 'b -> 'a array -> ('b -> 'a -> 'b) -> 'b

val single_of: 'a array -> 'a
val pair_of: 'a array -> 'a * 'a
val triple_of: 'a array -> 'a * 'a * 'a

type ('a, 'b) builder =
	| Cont of 'a
	| Done of 'b
val build_loop0_with_first: 'a -> (unit -> ('a, 'b) builder) -> 'a array * 'b
val build_loop0: (unit -> ('a, 'b) builder) -> 'a array * 'b

(* More specific builds*)
val build_and_return: (('a -> unit) -> 'b) -> 'a array * 'b
val build: (('a -> unit) -> unit) -> 'a array
val build_loop: (unit -> 'a * bool) -> 'a array
val build_until_none: (unit -> 'a option) -> 'a array
val build_until_none_with_first: 'a -> (unit -> 'a option) -> 'a array
val build_fold: 'f -> ('f -> 'a option * 'f option) -> 'a array

val try_remove: 'a array -> 'a -> 'a array option
val try_remove_where: 'a array -> ('a -> bool) -> ('a * 'a array) option

val partial: 'a array -> 'b array -> ('a -> 'b -> unit) -> 'a array

val rtail: 'a array -> 'a array
val last: 'a array -> 'a

val output_elements: ?delimeter:string -> ('a, 'o) OutputU.printer -> ('a array, 'o) OutputU.printer
val output: ('a, 'o) OutputU.printer -> ('a array, 'o) OutputU.printer

val eq: 'a array -> 'b array -> ('a -> 'b -> bool) -> bool
