open Ast

val expr_loc: expr -> Loc.t
val decl_loc_name: decl -> Loc.t * Sym.t

module AccessLookup: Lookup.S with type key = access
module ExprLookup: Lookup.S with type key = expr
module FnLookup: Lookup.S with type key = fn
module CnLookup: Lookup.S with type key = cn
module RtLookup: Lookup.S with type key = rt
module UnLookup: Lookup.S with type key = un
module FtLookup: Lookup.S with type key = ft
module CtLookup: Lookup.S with type key = ct
module LocalDeclareLookup: Lookup.S with type key = local_declare
module ParameterLookup: Lookup.S with type key = parameter

val output_access: (access, 'o) OutputU.printer
val output_fn: (fn, 'o) OutputU.printer
val output_ty: (ty, 'o) OutputU.printer
val output_parameter: (parameter, 'o) OutputU.printer
val output_local_declare: (local_declare, 'o) OutputU.printer
val output_expr: (expr, 'o) OutputU.printer
val output_decl_val: (decl_val, 'o) OutputU.printer
val output_decl_ty: (decl_ty, 'o) OutputU.printer
val output_decl: (decl, 'o) OutputU.printer
val output_modul: (modul, 'o) OutputU.printer
