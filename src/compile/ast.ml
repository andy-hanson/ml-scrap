type access = Loc.t * Sym.t

type typ =
	| TypeAccess of access
	(*| GenericTypeInstance*)

type local_declare = Loc.t * Sym.t

type case_test =
	| AsTest of Loc.t * local_declare * typ
type case_part = Loc.t * case_test * expr

and expr =
	| ExprAccess of access
	| Call of Loc.t * expr * expr array
	| Case of Loc.t * expr * case_part array
	| Let of Loc.t * local_declare * expr * expr
	| Literal of Loc.t * N.primitive
	| Seq of Loc.t * expr * expr
	| Partial of Loc.t * expr * expr array
	(* Head * (interpolation, text) pairs. For string with no interpolations, Literal is used instead. *)
	| Quote of Loc.t * string * quote_part array

and quote_part = expr * string

type property = Loc.t * Sym.t * typ
type parameter = Loc.t * Sym.t * typ
type signature = Loc.t * typ * parameter array

type fn = Loc.t * Sym.t * signature * expr
type cn = Loc.t * Sym.t * typ * case_part array
type rt = Loc.t * Sym.t * property array
type un = Loc.t * Sym.t * typ array
type ft = Loc.t * Sym.t * signature
type ct = Loc.t * Sym.t * ((typ * typ) array)
type decl =
	| Fn of fn
	| Cn of cn
	| Rt of rt
	| Un of un
	| Ft of ft
	| Ct of ct

type modul = decl array
