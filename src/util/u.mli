exception TODO

val do_times: int -> (unit -> unit) -> unit
val returning: 'a -> ('a -> unit) -> 'a
val op_if: bool -> (unit -> 'a) -> 'a option
val mod_ref: 'a ref -> ('a -> 'a) -> unit
val assert_equal: (string OutputU.t -> 'a -> unit) -> 'a -> 'a -> unit
