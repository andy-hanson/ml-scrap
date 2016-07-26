open Ast

module type SimpleKey = sig
	type t
	val loc: t -> Loc.t
end
module L(K: SimpleKey): Lookup.S with type key = K.t = Lookup.Make(struct
	type t = K.t
	let equal: t -> t -> bool = (==)
	let hash(ast: t): int = Loc.hash @@ K.loc ast
end)

module Ty = L(struct
	type t = ty
	let loc = AstU.ty_loc
end)

module Access = L(struct
	type t = access
	let loc(loc, _) = loc
end)
module Expr = L(struct
	type t = expr
	let loc = AstU.expr_loc
end)
module Fn = L(struct
	type t = fn
	let loc(loc, _, _, _) = loc
end)
module Rt = L(struct
	type t = rt
	let loc(loc, _, _) = loc
end)
module GenRt = L(struct
	type t = gen_rt
	let loc(loc, _, _, _) = loc
end)
module Un = L(struct
	type t = un
	let loc(loc, _, _) = loc
end)
module Ft = L(struct
	type t = ft
	let loc(loc, _, _) = loc
end)
module LocalDeclare = L(struct
	type t = local_declare
	let loc(loc, _) = loc
end)
module Parameter = L(struct
	type t = parameter
	let loc(loc, _, _) = loc
end)
