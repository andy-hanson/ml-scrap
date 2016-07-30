val output_message: (Err.message, 'o) OutputU.printer
val output: (Path.t -> Loc.t -> Loc.lc_loc) -> 'o OutputU.t -> Err.t -> unit
