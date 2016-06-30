type t
val make: (CompileError.t -> unit) -> t
val warn: t -> Loc.t -> CompileError.message -> unit
