open Ast

val ty_loc: ty -> Loc.t
val expr_loc: expr -> Loc.t
val fn_name: fn -> Sym.t
val decl_loc_name: decl -> Loc.t * Sym.t
