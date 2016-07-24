exception Empty
type 'a t

val create: unit -> 'a t
val of_array: 'a array -> 'a t

val get: 'a t -> int -> 'a
val length: 'a t -> int
val empty: 'a t -> bool

val to_array: 'a t -> 'a array

val blit: 'a t -> int -> 'a t -> int -> int -> unit
val set: 'a t -> int -> 'a -> unit
val push: 'a t -> 'a -> unit
val push_many: 'a t -> 'a array -> unit
val delete_last: 'a t -> unit
val delete_last_n: 'a t -> int -> unit
(* Takes start and length *)
val delete_range: 'a t -> int -> int -> unit
val remove: 'a t -> 'a -> ('a -> 'a -> bool) -> unit

val peek: 'a t -> 'a
val pop: 'a t -> 'a
val pop_n: 'a t -> int -> 'a array

val iter: 'a t -> ('a -> unit) -> unit
(* Takes start and length *)
val slice: 'a t -> int -> int -> 'a array
val tail: 'a t -> 'a array

val map_to_array: 'a t -> ('a -> 'b) -> 'b array

val un_let: 'a t -> int -> unit
val output_with_max: int -> ('a, 'o) OutputU.printer -> ('a t, 'o) OutputU.printer
