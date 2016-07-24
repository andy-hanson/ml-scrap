val print_tokens: Path.t -> unit
val lex: Path.t -> Token.t array
val parse: Path.t -> Ast.modul
val compile: Path.t -> N.modul
val time: (unit -> 'a) -> 'a
val call_fn: N.modul -> string -> N.v array -> N.v
