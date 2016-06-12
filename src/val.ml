type t =
	| Bool of bool
	| Float of float
	| Int of int
	| Rc of Type.rc * t array
	| Void
