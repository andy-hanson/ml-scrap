exception TODO

val compose: ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
val fail: string -> 'a
val do_times: int -> (unit -> unit) -> unit
val returning: 'a -> ('a -> unit) -> 'a
