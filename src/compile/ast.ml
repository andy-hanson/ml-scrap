type access = Loc.t * Sym.t
type local_declare = Loc.t * Sym.t

type typ =
	| TypeAccess of access
	(*| GenericTypeInstance*)

type pattern =
	| PSingle of local_declare
	| PDestruct of Loc.t * pattern array
(*TODO: just inline this...*)
type cs_test = Loc.t * typ * pattern
type cs_part = Loc.t * cs_test * expr

and expr =
	| At of Loc.t * typ * expr
	| ExprType of typ
	| ExprAccess of access
	| Call of Loc.t * expr * expr array
	| Cs of Loc.t * expr * cs_part array
	| Let of Loc.t * pattern * expr * expr
	| Literal of Loc.t * N.primitive
	| Seq of Loc.t * expr * expr
	| Partial of Loc.t * expr * expr array
	(* Head * (interpolation, text) pairs. For string with no interpolations, Literal is used instead. *)
	| Quote of Loc.t * string * quote_part array
	| Check of Loc.t * expr

and quote_part = expr * string

type property = Loc.t * Sym.t * typ
type parameter = Loc.t * Sym.t * typ
type signature = Loc.t * typ * parameter array

type fn = Loc.t * Sym.t * signature * expr
type cn = Loc.t * Sym.t * typ * cs_part array
type rt = Loc.t * Sym.t * property array
type un = Loc.t * Sym.t * typ array
type ft = Loc.t * Sym.t * signature
type ct = Loc.t * Sym.t * ((typ * typ) array)

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
