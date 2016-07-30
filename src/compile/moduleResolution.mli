(* Returns: logical path, full path, ast *)
val linearize_modul_dependencies: FileIo.t -> Path.t -> (Path.t * Path.t * Ast.modul) array
