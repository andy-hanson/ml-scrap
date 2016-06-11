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

val try_pop: 'a t -> 'a option

val output: ('a, 'o) OutputU.printer -> 'o OutputU.t -> 'a t -> unit
