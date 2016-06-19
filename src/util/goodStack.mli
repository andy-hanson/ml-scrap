exception EmptyStack

type 'a t

val create: unit -> 'a t

val get: 'a t -> int -> 'a

val size: 'a t -> int

val empty: 'a t -> bool

val push: 'a t -> 'a -> unit

val peek: 'a t -> 'a

val pop: 'a t -> 'a
val pop_n: 'a t -> int -> 'a array
val un_let: 'a t -> unit

val output_with_max: int -> ('a, 'o) OutputU.printer -> 'o OutputU.t -> 'a t -> unit
