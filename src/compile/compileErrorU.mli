open CompileError

val raise: Loc.t -> message -> 'a
val check: bool -> Loc.t -> message -> unit
val output_message: (message, 'o) OutputU.printer
