type t

val create: Val.fn -> t

val peek: t -> Val.t
val pop: t -> Val.t
val pop_n: t -> int -> Val.t array
val push: t -> Val.t -> unit
val un_let: t -> unit

val cur_fn: t -> Val.fn
val cur_code: t -> Val.bytecode

val goto: t -> int -> unit
val goto_next: t -> unit

val push_fn: t -> Val.fn -> unit
(* Returns true iff there is nothing left.*)
val pop_fn: t -> bool

(*Takes *relative* index*)
val load: t -> int -> Val.t

val assert_data_stack_back_to_function_start: t -> unit

val debug_print: Noze.t -> FileIO.file_name -> t -> unit
