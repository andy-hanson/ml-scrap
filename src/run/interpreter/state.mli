open N

val create: declared_fn -> v array -> interpreter_state

val peek: interpreter_state -> v
val pop: interpreter_state -> v
val pop_n: interpreter_state -> int -> v array
val push: interpreter_state -> v -> unit
val un_let: interpreter_state -> unit

val cur_fn: interpreter_state -> declared_fn
val cur_code: interpreter_state -> bytecode
val cur_loc: interpreter_state -> Loc.t

val goto: interpreter_state -> int -> unit
val goto_next: interpreter_state -> unit

val push_fn: interpreter_state -> declared_fn -> unit
(* Returns true iff there is nothing left.*)
val pop_fn: interpreter_state -> bool

(*Takes *relative* index*)
val load: interpreter_state -> int -> v

val call_builtin: interpreter_state -> builtin_fn -> unit
val call_lambda: interpreter_state -> v -> unit

val assert_data_stack_back_to_function_start: interpreter_state -> unit
