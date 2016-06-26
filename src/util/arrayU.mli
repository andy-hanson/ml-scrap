val empty: 'a array -> bool

val head: 'a array -> 'a
val tail: 'a array -> 'a array

val same_length: 'a array -> 'b array -> bool

val iter: 'a array -> ('a -> unit) -> unit
val iteri: 'a array -> (int -> 'a -> unit) -> unit
val iter_zip: 'a array -> 'b array -> ('a -> 'b -> unit) -> unit

val map: 'a array -> ('a -> 'b) -> 'b array
val mapi: 'a array -> (int -> 'a -> 'b) -> 'b array
val map_zip: 'a array -> 'b array -> ('a -> 'b -> 'c) -> 'c array
val zip: 'a array -> 'b array -> ('a * 'b) array

val fold_map: 'b -> 'a array -> ('b -> 'a -> 'b * 'c) -> 'b * 'c array

val filter: 'a array -> ('a -> bool) -> 'a array
val filter_map: 'a array -> ('a -> 'b option) -> 'b array

val find: 'a array -> ('a -> bool) -> 'a option
val find_map: 'a array -> ('a -> 'b option) -> 'b
val exists: 'a array -> ('a -> bool) -> bool
val for_all_zip: 'a array -> 'b array -> ('a -> 'b -> bool) -> bool

val fold: 'b -> 'a array -> ('b -> 'a -> 'b) -> 'b

val single_of: 'a array -> 'a
val pair_of: 'a array -> 'a * 'a
val triple_of: 'a array -> 'a * 'a * 'a

val build: (('a -> unit) -> unit) -> 'a array
val build_loop: (unit -> 'a * bool) -> 'a array
val build_until_none: (unit -> 'a option) -> 'a array

val try_remove: 'a array -> 'a -> 'a array option
val try_remove_where: 'a array -> ('a -> bool) -> ('a * 'a array) option
