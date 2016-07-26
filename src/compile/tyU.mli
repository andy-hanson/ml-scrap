open N.Ty

val name: ty -> Sym.t

val ft_or_gen_arity: ft_or_gen -> int
val rt_arity: rt -> int

val partial: ft -> int -> ft

val ty_of_ft_or_gen: ft_or_gen -> ty
