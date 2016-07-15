open N

val name: ty -> Sym.t

val ft: Sym.t -> ty -> parameter array -> ft
val t_ft: Sym.t -> ty -> parameter array -> ty
val t_rc: Sym.t -> property array -> ty

val arity: ty -> int
val ft_arity: ft -> int
val rt_arity: rt -> int

val partial_ty: ty_fn -> ty array -> ft
val ct_input: ct -> un
val ct_output: ct -> un

(*TODO: NOut module*)
val output_property: (property, 'o) OutputU.printer
val output_ft: (ft, 'o) OutputU.printer
val output_ct: (ct, 'o) OutputU.printer
val output_ty_fn: (ty_fn, 'o) OutputU.printer
val output_rt: (rt, 'o) OutputU.printer
val output: (ty, 'o) OutputU.printer
val output_brief: (ty, 'o) OutputU.printer
