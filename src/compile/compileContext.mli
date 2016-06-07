type t
val make: unit -> t
val symbol: t -> string -> Symbol.t
val keyword: t -> Symbol.t -> Token.t option
val warn: t -> Loc.t -> CompileError.message -> unit
