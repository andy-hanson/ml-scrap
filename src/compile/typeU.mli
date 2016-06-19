open Type

val name: t -> Sym.t

val is_subtype: t -> t -> bool

val ft: Sym.t -> t -> parameter array -> ft
val t_ft: Sym.t -> t -> parameter array -> t
val property: Sym.t -> t -> property
val t_rc: Sym.t -> property array -> t

val ft_arity: ft -> int
val rc_arity: rc -> int

val output_property: (property, 'o) OutputU.printer
val output_ft: (ft, 'o) OutputU.printer
val output_rc: (rc, 'o) OutputU.printer
val output: (t, 'o) OutputU.printer
