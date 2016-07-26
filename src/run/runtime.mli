open N.V
open N.Compiler
open N.Run

val create: FileIo.t -> runtime
val compile: runtime -> Path.t -> modul
val add_thread: runtime -> declared_fn -> v array -> unit
val run: runtime -> v
