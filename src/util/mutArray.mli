type 'a t

val create: unit -> 'a t
val of_array: 'a array -> 'a t

val get: 'a t -> int -> 'a
val last: 'a t -> 'a
val length: 'a t -> int
val empty: 'a t -> bool

val to_array: 'a t -> 'a array

val blit: 'a t -> int -> 'a t -> int -> int -> unit
val set: 'a t -> int -> 'a -> unit
val add: 'a t -> 'a -> unit
val delete_last: 'a t -> unit
val delete_last_n: 'a t -> int -> unit
(* Takes start and length *)
val delete_range: 'a t -> int -> int -> unit

val iter: 'a t -> ('a -> unit) -> unit
(* Takes start and length *)
val slice: 'a t -> int -> int -> 'a array
val tail: 'a t -> 'a array

val map_to_array: 'a t -> ('a -> 'b) -> 'b array
