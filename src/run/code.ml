type bytecode =
	| Call of fn
	| CallBuiltin of Builtin.t
	| Case of (Type.t * int) array
	| Const of Val.t
	| Construct of Type.rc
	| Drop
	(* Load a value from `int` entries earlier in the stack *)
	| Load of int
	(* Goto-like codes store index of bytecode to move to. *)
	| Goto of int
	| GotoIfFalse of int
	| Return
	(* For `a = b; c`, we push `b`, then eval `c` (which may fetch `a`); then remove `a` from under it. *)
	| UnLet
and t = bytecode array
and parameter = {
	param_name: Symbol.t;
	param_type: Type.t
}
and fn = {
	fname: Symbol.t;
	return_type: Type.t;
	params: parameter array;
	mutable code: t
}
