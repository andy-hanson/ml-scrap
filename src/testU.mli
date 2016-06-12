val print_tokens: string -> unit
val lex: string -> Token.t array
val parse: string -> Ast.modul
val compile: string -> Modul.t
val time: (unit -> 'a) -> 'a
val symbol: string -> Symbol.t
