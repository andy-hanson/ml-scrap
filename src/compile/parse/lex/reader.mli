type t

val make: BatIO.input -> t

val pos: t -> Loc.pos

val peek: t -> char

val next: t -> char

val try_eat_if: t -> (char -> bool) -> bool

val try_eat: t -> char -> bool

val skip_rest_of_line: t -> unit
val take_rest_of_line: t -> string

val take_num_decimal: bool -> char -> t -> Token.t

val take_name_like: char -> t -> string

val skip_newlines: t -> unit

val skip_tabs: t -> int
