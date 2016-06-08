val print_tokens: string -> unit
val parse: string -> Ast.modul
val compile: string -> Modul.t
val time: (unit -> 'a) -> 'a
val symbol: string -> Symbol.t
