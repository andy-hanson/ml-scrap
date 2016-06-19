type t
val make: (CompileError.warning -> unit) -> t
val warn: t -> Loc.t -> CompileError.message -> unit
