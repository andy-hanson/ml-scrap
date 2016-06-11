module ExprLookup: Lookup.S with type key = Ast.expr
module TypLookup: Lookup.S with type key = Ast.typ
module DeclLookup: Lookup.S with type key = Ast.decl
module FnLookup: Lookup.S with type key = Ast.fn
module RcLookup: Lookup.S with type key = Ast.rc
module LocalDeclareLookup: Lookup.S with type key = Ast.local_declare
module ParameterLookup: Lookup.S with type key = Ast.parameter

val modul_rcs: Ast.decl array -> Ast.rc array

val output_typ: (Ast.typ, 'o) OutputU.printer
val output_parameter: (Ast.parameter, 'o) OutputU.printer
val output_local_declare: (Ast.local_declare, 'o) OutputU.printer
val output_expr: (Ast.expr, 'o) OutputU.printer
val output_decl: (Ast.decl, 'o) OutputU.printer
val output_modul: (Ast.modul, 'o) OutputU.printer
