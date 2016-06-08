type t

val make: string -> t
val string_of: t -> string

val output: 'a OutputU.t -> t -> unit

val eq: t -> t -> bool

module SymMap: Map.S with type key = t
