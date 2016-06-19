type t

val create: FileIO.t -> t

val lex: t -> FileIO.file_name -> (Token.t * Loc.t) array
val parse: t -> FileIO.file_name -> Ast.modul
val compile: t -> FileIO.file_name -> Val.modul

val lc_loc: t -> FileIO.file_name -> Loc.t -> Loc.lc_loc
