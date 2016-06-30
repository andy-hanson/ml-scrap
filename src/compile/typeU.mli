open N

val name: ty -> Sym.t

val ft: Sym.t -> ty -> parameter array -> ft
val t_ft: Sym.t -> ty -> parameter array -> ty
val t_rc: Sym.t -> property array -> ty

val arity: ty -> int
val ft_arity: ft -> int
val rt_arity: rt -> int

val partial_type: ty_fn -> ty array -> ft

(*TODO: NOut module*)
val output_property: (property, 'o) OutputU.printer
val output_ft: (ft, 'o) OutputU.printer
val output_ct: (ct, 'o) OutputU.printer
val output_ty_fn: (ty_fn, 'o) OutputU.printer
val output_rc: (rt, 'o) OutputU.printer
val output: (ty, 'o) OutputU.printer
