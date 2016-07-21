(* Must bundle it in a record because Ocaml makes me. *)
type err = { err: 'a. Path.t -> Err.t -> 'a }
(* Returns: logical path, full path, ast *)
val linearize_modul_dependencies: FileIo.t -> err -> Path.t -> (Path.t * Path.t * Ast.modul) array Lwt.t
