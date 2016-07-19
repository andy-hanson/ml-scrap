type t = Sym.t array
type rel = int * Sym.t array

val to_string: t -> string
val of_string: string -> t
val output: (t, 'o) OutputU.printer
val output_rel: (rel, 'o) OutputU.printer

val resolve: t -> rel -> t
val add: t -> Sym.t -> t
val add_extension: t -> string -> t

module Lookup: Lookup.S with type key = t
