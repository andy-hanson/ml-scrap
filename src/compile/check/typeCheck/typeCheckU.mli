open N

val eq: ty -> ty -> bool
val assert_eq: Loc.t -> ty -> ty -> unit
(*
Whether we can do `foo A = bar`.
This is more permissive than `assert_parameter_assignable` since the type declaration and the value are in the same code together.
*)
val assert_value_assignable: Loc.t -> ty -> ty -> unit
(*
Whether we can do `foo bar` where `foo` takes A and `bar` is of type `B`.
Unlike `assert_value_assignable`, this does not allow upcasting functions.
*)
val assert_parameter_assignable: Loc.t -> ty -> ty -> unit
val assert_fn_upcast: Loc.t -> ty_fn -> ty -> unit
val join: Loc.t -> ty array -> ty
