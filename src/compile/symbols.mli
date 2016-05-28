type t

val get: t -> string -> Symbol.t

val keyword: t -> Symbol.t -> Token.t option

val create: unit -> t
