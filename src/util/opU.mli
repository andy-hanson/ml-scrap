val op_if: bool -> (unit -> 'a) -> 'a option
val may: 'a option -> ('a -> unit) -> unit
