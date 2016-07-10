open Ast

type t
val bind: modul -> t
(*TODO: it's easy to forget which of these to call...*)
val binding: t -> access -> Binding.v
val ty_binding: t -> access -> Binding.ty
val output: (t, 'o) OutputU.printer
