val iter: 'a array -> ('a -> unit) -> unit
val iteri: 'a array -> (int -> 'a -> unit) -> unit
val iter_zip: 'a array -> 'b array -> ('a -> 'b -> unit) -> unit

val map: 'a array -> ('a -> 'b) -> 'b array

val fold: 'b -> 'a array -> ('b -> 'a -> 'b) -> 'b

val triple_of_array: 'a array -> 'a * 'a * 'a

val build: (('a -> unit) -> unit) -> 'a array
val build_loop: (unit -> 'a * bool) -> 'a array
val build_until_none: (unit -> 'a option) -> 'a array
