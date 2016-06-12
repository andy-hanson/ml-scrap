type access = Access of Loc.t * Symbol.t

type typ =
	| TypeAccess of access
	| Or of Loc.t * typ array

type local_declare = LocalDeclare of Loc.t * Symbol.t

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

type property = Property of Loc.t * Symbol.t * typ

type parameter = Parameter of Loc.t * Symbol.t * typ

type signature = Signature of Loc.t * typ * parameter array

type fn = Fn of Loc.t * Symbol.t * signature * expr
type rc = Rc of Loc.t * Symbol.t * property array
type decl =
	| DeclFn of fn
	| DeclRc of rc

type modul = Modul of decl array
