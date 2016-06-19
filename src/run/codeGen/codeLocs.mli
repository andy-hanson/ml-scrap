type t
(* Get the loc for the bytecode at the given index. *)
val get: t -> int -> Loc.t
val empty: t

type builder
val create_builder: unit -> builder
val write: builder -> Loc.t -> unit
val finish: builder -> t

val output: (t, 'o) OutputU.printer
