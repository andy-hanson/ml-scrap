val test_noze: Noze.t
val print_tokens: string -> unit
val lex: string -> Token.t array
val parse: string -> Ast.modul
val compile: string -> Val.modul
val time: (unit -> 'a) -> 'a
