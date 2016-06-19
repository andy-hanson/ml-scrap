type t =
	| Bool of bool
	| Float of float
	| Int of int
	(* TODO: have fn, closure, and builtin, but have them all act the same way. *)
	| Fn of closure_fn * t array
	| BuiltinFn of builtin_fn
	| Rc of Type.rc * t array
	| Void

and builtin_fn = {
	(*TODO:RENAME*)
	typ: Type.ft;
	(* Takes the 'pop' function *)
	exec: (unit -> t) -> t
}

and bytecode =
	| CallStatic of fn
	(*TODO: kill and just have CallLambda?*)
	| CallBuiltin of builtin_fn
	| CallLambda
	| Case of (Type.t * int) array
	| Const of t
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

and code = {
	bytecodes: bytecode array;
	locs: CodeLocs.t
}
and closure_fn = {
	(*TODO: info about closure values?*)
	closure_info: fn_info
}
and fn = {
	info: fn_info
}
and fn_info = {
	ft: Type.ft;
	containing_modul: modul;
	mutable code: code
}
and modul = {
	file_name: string;
	mutable fns: fn array;
	rcs: Type.rc array;
	uns: Type.un array;
	fts: Type.ft array
}
