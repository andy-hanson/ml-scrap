type access = Loc.t * Sym.t
type local_declare = Loc.t * Sym.t

type ty =
	| TypeAccess of access
	(*| GenericTypeInstance*)

type pattern =
	| PSingle of local_declare
	| PDestruct of Loc.t * pattern array

type at_kind =
	| Exact
	| Convert

type expr =
	| At of Loc.t * at_kind * ty * expr
	| ExprType of ty
	| ExprAccess of access
	| Call of Loc.t * expr * expr array
	| Cs of Loc.t * expr * cs_part array
	| GetProperty of Loc.t * expr * Sym.t
	| Let of Loc.t * pattern * expr * expr
	| Literal of Loc.t * N.primitive
	| Seq of Loc.t * expr * expr
	| Partial of Loc.t * expr * expr array
	(* Head * (interpolation, text) pairs. For string with no interpolations, Literal is used instead. *)
	| Quote of Loc.t * string * quote_part array
	| Check of Loc.t * expr

and cs_test = Loc.t * ty * pattern
and cs_part = Loc.t * cs_test * expr
and quote_part = expr * string

type property = Loc.t * Sym.t * ty
type parameter = Loc.t * Sym.t * ty
type signature = Loc.t * ty * parameter array

type fn = Loc.t * Sym.t * signature * expr
type cn = Loc.t * Sym.t * ty * cs_part array
type rt = Loc.t * Sym.t * property array
type un = Loc.t * Sym.t * ty array
type ft = Loc.t * Sym.t * signature
type ct = Loc.t * Sym.t * ((ty * ty) array)

type decl_val =
	| Fn of fn
	| Cn of cn
type decl_ty =
	| Rt of rt
	| Un of un
	| Ft of ft
	| Ct of ct
type decl =
	| DeclVal of decl_val
	| DeclTy of decl_ty

type imports = unit
type modul = imports * decl array
