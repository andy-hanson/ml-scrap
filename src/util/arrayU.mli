val same_length: 'a array -> 'b array -> bool

val iter: 'a array -> ('a -> unit) -> unit
val iteri: 'a array -> (int -> 'a -> unit) -> unit
val iter_zip: 'a array -> 'b array -> ('a -> 'b -> unit) -> unit

val map: 'a array -> ('a -> 'b) -> 'b array
val mapi: 'a array -> (int -> 'a -> 'b) -> 'b array
val map_zip: 'a array -> 'b array -> ('a -> 'b -> 'c) -> 'c array

val exists: 'a array -> ('a -> bool) -> bool
val find_map: 'a array -> ('a -> 'b option) -> 'b

val fold: 'b -> 'a array -> ('b -> 'a -> 'b) -> 'b

val triple_of_array: 'a array -> 'a * 'a * 'a

val build: (('a -> unit) -> unit) -> 'a array
val build_loop: (unit -> 'a * bool) -> 'a array
val build_until_none: (unit -> 'a option) -> 'a array
