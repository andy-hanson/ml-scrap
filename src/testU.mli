val test_noze: Noze.t
val print_tokens: string -> unit
val lex: string -> Token.t array
val parse: string -> Ast.modul
val compile: string -> N.modul
val time: (unit -> 'a) -> 'a
val fn_named: N.modul -> Sym.t -> N.fn
val call_fn: Noze.t -> N.modul -> Sym.t -> N.v array -> N.v
