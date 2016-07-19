exception TODO

val compose: ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
val fail: string -> 'a
val do_times: int -> (unit -> unit) -> unit
val returning: 'a -> ('a -> unit) -> 'a

val loop0: ((unit -> 'res) -> 'res) -> 'res
val loop: 'state -> (('state -> 'res) -> 'state -> 'res) -> 'res
val loop2: 'a -> 'b -> (('a -> 'b -> 'res) -> 'a -> 'b -> 'res) -> 'res
val loop3: 'a -> 'b -> 'c -> (('a -> 'b -> 'c -> 'res) -> 'a -> 'b -> 'c -> 'res) -> 'res
