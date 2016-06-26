type access = Loc.t * Sym.t

type typ =
	| TypeAccess of access
	(*GenericTypeInstance*)

type local_declare = Loc.t * Sym.t

type case_test =
	(* :foo Foo *)
	| AsTest of Loc.t * local_declare * typ
type case_part = Loc.t * case_test * expr

and expr =
	| ExprAccess of access
	| Call of Loc.t * expr * expr array
	| Case of Loc.t * expr * case_part array
	| Let of Loc.t * local_declare * expr * expr
	| Literal of Loc.t * N.v
	| Seq of Loc.t * expr * expr

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
	(*TODO: take out 'Decl' from names*)
	| DeclFn of fn
	| DeclCn of cn
	| DeclRt of rt
	| DeclUn of un
	| DeclFt of ft
	| DeclCt of ct

(*TODO: use tuple*)
type modul = Modul of decl array
