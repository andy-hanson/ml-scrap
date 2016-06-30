val force: 'a option -> 'a
val op_if: bool -> (unit -> 'a) -> 'a option
val may: 'a option -> ('a -> unit) -> unit
val map: 'a option -> ('a -> 'b) -> 'b option
val or_else: 'a option -> (unit -> 'a) -> 'a
val or_try: 'a option -> (unit -> 'a option) -> 'a option
