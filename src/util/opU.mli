val op_if: bool -> (unit -> 'a) -> 'a option
val may: 'a option -> ('a -> unit) -> unit
val or_else: 'a option -> (unit -> 'a) -> 'a
