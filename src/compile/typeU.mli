open N

val name: ty -> Sym.t

val is_subtype: ty -> ty -> bool

val ft: Sym.t -> ty -> parameter array -> ft
val t_ft: Sym.t -> ty -> parameter array -> ty
val t_rc: Sym.t -> property array -> ty

val arity: ty -> int
val ft_arity: ft -> int
val rt_arity: rt -> int

(*TODO: NOut module*)
val output_property: (property, 'o) OutputU.printer
val output_ft: (ft, 'o) OutputU.printer
val output_ct: (ct, 'o) OutputU.printer
val output_fn_type: (fn_type, 'o) OutputU.printer
val output_rc: (rt, 'o) OutputU.printer
val output: (ty, 'o) OutputU.printer
