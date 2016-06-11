type t

val make: string -> t
val string_of: t -> string

val output: 'o OutputU.t -> t -> unit

val eq: t -> t -> bool

module Map: MapU.S with type key = t

module Lookup: Lookup.S with type key = t
