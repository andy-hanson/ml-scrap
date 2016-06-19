type access = Access of Loc.t * Sym.t

type typ =
	| TypeAccess of access
	(*TODO:KILL!!!!*)
	| TypeFn of Loc.t * typ * typ array

type local_declare = LocalDeclare of Loc.t * Sym.t

type case_test =
	(* :foo Foo *)
	| AsTest of Loc.t * local_declare * typ
type case_part = CasePart of Loc.t * case_test * expr
and expr =
	| ExprAccess of access
	| Call of Loc.t * expr * expr array
	| Case of Loc.t * expr * case_part array
	| Let of Loc.t * local_declare * expr * expr
	| Literal of Loc.t * Val.t
	| Seq of Loc.t * expr * expr

type property = Property of Loc.t * Sym.t * typ

type parameter = Parameter of Loc.t * Sym.t * typ

type signature = Signature of Loc.t * typ * parameter array

type fn = Fn of Loc.t * Sym.t * signature * expr
type rc = Rc of Loc.t * Sym.t * property array
type un = Un of Loc.t * Sym.t * typ array
type ft = Ft of Loc.t * Sym.t * signature
type decl =
	| DeclFn of fn
	| DeclRc of rc
	| DeclUn of un
	| DeclFt of ft

type modul = Modul of decl array
