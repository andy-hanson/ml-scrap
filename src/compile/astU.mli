open Ast

val typ_loc: typ -> Loc.t
val expr_loc: expr -> Loc.t
val decl_loc_name: decl -> Loc.t * Symbol.t

module AccessLookup: Lookup.S with type key = access
module ExprLookup: Lookup.S with type key = expr
module FnLookup: Lookup.S with type key = fn
module RcLookup: Lookup.S with type key = rc
module LocalDeclareLookup: Lookup.S with type key = local_declare
module ParameterLookup: Lookup.S with type key = parameter

val modul_split: decl array -> fn array * rc array
val modul_fns: decl array -> fn array

val output_access: (access, 'o) OutputU.printer
val output_typ: (typ, 'o) OutputU.printer
val output_parameter: (parameter, 'o) OutputU.printer
val output_local_declare: (local_declare, 'o) OutputU.printer
val output_expr: (expr, 'o) OutputU.printer
val output_decl: (decl, 'o) OutputU.printer
val output_modul: (modul, 'o) OutputU.printer
