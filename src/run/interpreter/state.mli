open N.V
open N.Code
open N.Run

val create: declared_fn -> v array -> interpreter_state

val peek: interpreter_state -> v
val pop: interpreter_state -> v
val pop_n: interpreter_state -> int -> v array
val drop_to_start_of_fn: interpreter_state -> unit
val push: interpreter_state -> v -> unit
val un_let: interpreter_state -> int -> unit

val cur_fn: interpreter_state -> declared_fn
val cur_code: interpreter_state -> bytecode
val cur_loc: interpreter_state -> Loc.t

val goto: interpreter_state -> int -> unit
val goto_next: interpreter_state -> unit

val pop_fn: interpreter_state -> bool

(*Takes *relative* index*)
val load: interpreter_state -> int -> v

val call: interpreter_state -> v -> unit
