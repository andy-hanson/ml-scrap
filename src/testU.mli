val test_noze: Noze.t
val print_tokens: Path.t -> unit Lwt.t
val lex: Path.t -> Token.t array Lwt.t
val parse: Path.t -> Ast.modul Lwt.t
val compile: Path.t -> N.modul Lwt.t
val time: (unit -> 'a) -> 'a
val fn_named: N.modul -> string -> N.declared_fn
val call_fn: Noze.t -> N.modul -> string -> N.v array -> N.v
