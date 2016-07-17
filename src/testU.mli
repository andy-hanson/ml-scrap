val test_noze: Noze.t
val print_tokens: Path.t -> unit
val lex: Path.t -> Token.t array
val parse: Path.t -> Ast.modul
val compile: Path.t -> N.modul
val time: (unit -> 'a) -> 'a
val fn_named: N.modul -> string -> N.declared_fn
val call_fn: Noze.t -> N.modul -> string -> N.v array -> N.v
