open N

val eq: ty -> ty -> bool
val assert_eq: Loc.t -> ty -> ty -> unit
(*
Allows the second ty to be *exactly* the first, or if the first is a union, a member of it.
*)
val assert_exact: Loc.t -> ty -> ty -> unit
(*
More permissive than `assert_exact`.
Allows the second ty to be convertible to the first.
*)
val assert_convert: Loc.t -> ty -> ty -> unit
val assert_upcast: Loc.t -> ty -> ty -> unit
val join: Loc.t -> ty array -> ty
