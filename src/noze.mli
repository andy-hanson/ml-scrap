type t

val create: FileIO.t -> t

val lex: t -> string -> (Token.t * Loc.t) array
val parse: t -> string -> Ast.modul
val compile: t -> string -> Modul.t

val symbol: t -> string -> Symbol.t
