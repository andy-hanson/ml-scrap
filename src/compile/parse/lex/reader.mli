type t

val make: string -> t

type restore
val get_restore: t -> restore
val do_restore: t -> restore -> unit

val pos: t -> Loc.pos

val undo_read_newline: t -> unit

val can_peek: t -> int -> bool

val peek: t -> char

val peek_by: t -> int -> char

val skip: t -> unit

(*TODO: rename to `next`*)
val next: t -> char

val try_eat_if: t -> (char -> bool) -> bool

val try_eat: t -> char -> bool

val skip_rest_of_line: t -> unit
val take_rest_of_line: t -> string

val take_num_decimal: t -> Val.t

val take_name_like: t -> string

val skip_newlines: t -> unit

val skip_tabs: t -> int

