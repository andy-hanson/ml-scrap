type t

val of_string: string -> t
val of_buffer: BatBuffer.t -> t
val string_of: t -> string

val output: 'o OutputU.t -> t -> unit

val eq: t -> t -> bool
val hash: t -> int

module Map: MapU.S with type key = t

module Lookup: Lookup.S with type key = t
