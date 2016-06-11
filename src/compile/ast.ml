type typ =
	| TypeAccess of Loc.t * Symbol.t

type local_declare = LocalDeclare of Loc.t * Symbol.t

type expr =
	| Access of Loc.t * Symbol.t
	| Call of Loc.t * expr * expr array
	| Let of Loc.t * local_declare * expr * expr
	| Literal of Loc.t * Val.t
	| Seq of Loc.t * expr * expr
let expr_loc = function
	| Access(loc, _) -> loc
	| Call(loc, _, _) -> loc
	| Let(loc, _, _, _) -> loc
	| Literal(loc, _) -> loc
	| Seq(loc, _, _) -> loc

type property = Property of Loc.t * Symbol.t * typ

type parameter = Parameter of Loc.t * Symbol.t * typ

type signature = Signature of Loc.t * typ * parameter array

type fn = Fn of Loc.t * Symbol.t * signature * expr
type rc = Rc of Loc.t * Symbol.t * property array
type decl =
	| DeclFn of fn
	| DeclRc of rc

let decl_loc_name = function
	| DeclFn(Fn(loc, name, _, _)) ->
		loc, name
	| DeclRc(Rc(loc, name, _)) ->
		loc, name

type modul = Modul of decl array
