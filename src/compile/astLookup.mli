open Ast

(*TODO: maybe there's a way to reduce repetition here?*)
module Ty: Lookup.S with type key = ty

module Access: Lookup.S with type key = access
module Expr: Lookup.S with type key = expr
module Fn: Lookup.S with type key = fn
module Rt: Lookup.S with type key = rt
module GenRt: Lookup.S with type key = gen_rt
module Un: Lookup.S with type key = un
module Ft: Lookup.S with type key = ft
module LocalDeclare: Lookup.S with type key = local_declare
module Parameter: Lookup.S with type key = parameter
