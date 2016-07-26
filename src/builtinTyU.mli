open N.Ty

val ft: Sym.t -> ty -> parameter array -> ft
val t_ft: Sym.t -> ty -> parameter array -> ty
val t_rc: Sym.t -> property array -> ty

val ft0: string -> ty -> ty
val ft1: string -> ty -> string -> ty -> ty

(*TODO: rt_n, rt1*)
val rc_n: string -> (string * ty) array -> ty
val rc1: string -> string -> ty -> ty
