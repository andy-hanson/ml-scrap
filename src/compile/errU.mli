open Err

val raise_with_path: Path.t -> Loc.t -> message -> 'a
val raise: Loc.t -> message -> 'a
val add_path: Path.t -> t -> unit
val check: bool -> Loc.t -> message -> unit
