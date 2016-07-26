type access = Loc.t * Sym.t
type local_declare = Loc.t * Sym.t
type ty_param = Loc.t * Sym.t

type ty =
	| TyAccess of access
	| TyInst of ty_inst
and ty_inst = Loc.t * ty * ty array

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

type fn_head =
	| FnPlain of Sym.t
	| FnGeneric of Sym.t * ty_param array
type fn = Loc.t * fn_head * signature * expr
type rt = Loc.t * Sym.t * property array
type gen_rt = Loc.t * Sym.t * ty_param array * property array
type un = Loc.t * Sym.t * ty array
type ft = Loc.t * Sym.t * signature
type gen_ft = Loc.t * Sym.t * ty_param array * property array

type decl_val =
	| Fn of fn
type decl_ty =
	| Rt of rt
	| GenRt of gen_rt
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
