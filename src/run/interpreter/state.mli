type t

val create: N.fn -> N.v array -> t

val peek: t -> N.v
val pop: t -> N.v
val pop_n: t -> int -> N.v array
val push: t -> N.v -> unit
val un_let: t -> unit

val cur_fn: t -> N.fn
val cur_code: t -> N.bytecode

val goto: t -> int -> unit
val goto_next: t -> unit

val push_fn: t -> N.fn -> unit
(* Returns true iff there is nothing left.*)
val pop_fn: t -> bool

(*Takes *relative* index*)
val load: t -> int -> N.v

val assert_data_stack_back_to_function_start: t -> unit

val debug_print: Noze.t -> FileIO.file_name -> t -> unit
