type access = Loc.t * Sym.t
type local_declare = Loc.t * Sym.t

type ty =
	| TyAccess of access
	(* Generic type instantiation *)
	| TyInst of Loc.t * ty * ty array
	(*| GenericTypeInstance*)

type pattern =
	| PSingle of local_declare
	| PDestruct of Loc.t * pattern array

type at_kind =
	| Exact
	| Convert

type literal_value =
	| Int of int
	| Float of float
	| String of string

type expr =
	| At of Loc.t * at_kind * ty * expr
	| ExprType of ty
	| ExprAccess of access
	| Call of Loc.t * expr * expr array
	| Cs of Loc.t * expr * cs_part array
	| GetProperty of Loc.t * expr * Sym.t
	| Let of Loc.t * pattern * expr * expr
	| Literal of Loc.t * literal_value
	| Seq of Loc.t * expr * expr
	| Partial of Loc.t * expr * expr array
	(* Head * (interpolation, text) pairs. For string with no interpolations, Literal is used instead. *)
	| Quote of Loc.t * string * quote_part array
	| Check of Loc.t * expr
	(* Instantiate a value of generic type *)
	| GenInst of Loc.t * expr * ty array

and cs_test = Loc.t * ty * pattern
and cs_part = Loc.t * cs_test * expr
and quote_part = expr * string

type property = Loc.t * Sym.t * ty
type parameter = Loc.t * Sym.t * ty
type signature = Loc.t * ty * parameter array

type ty_name =
	| Plain of Sym.t
	| Generic of Sym.t * local_declare array
type fn = Loc.t * Sym.t(*TODO:ty_name*) * signature * expr
type rt = Loc.t * Sym.t(*TODO:ty_name*) * property array
type un = Loc.t * Sym.t * ty array
type ft = Loc.t * ty_name * signature

type decl_val =
	| Fn of fn
type decl_ty =
	| Rt of rt
	| Un of un
	| Ft of ft
type decl =
	| DeclVal of decl_val
	| DeclTy of decl_ty

(* Imported values for a single module. *)
type import_path =
	| Global of Sym.t array
	| Relative of Path.rel
type imports = Loc.t * import_path * local_declare array
type modul = imports array * decl array
