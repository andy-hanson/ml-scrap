open Ast

val output_access: (access, 'o) OutputU.printer
val output_fn: (fn, 'o) OutputU.printer
val output_ty: (ty, 'o) OutputU.printer
val output_parameter: (parameter, 'o) OutputU.printer
val output_ty_param: (ty_param, 'o) OutputU.printer
val output_local_declare: (local_declare, 'o) OutputU.printer
val output_literal: (literal_value, 'o) OutputU.printer
val output_expr: (expr, 'o) OutputU.printer
val output_decl_val: (decl_val, 'o) OutputU.printer
val output_decl_ty: (decl_ty, 'o) OutputU.printer
val output_decl: (decl, 'o) OutputU.printer
val output_modul: (modul, 'o) OutputU.printer
