open Err

val raise: Loc.t -> message -> 'a
val check: bool -> Loc.t -> message -> unit
