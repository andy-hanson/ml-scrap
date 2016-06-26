type ty =
	| Any
	| TBool
	| TFloat
	| TInt
	| TVoid (*TODO:RENAME*)
	| Rt of rt
	| Un of un
	| Ft of ft
	| Ct of ct

and property = Sym.t * ty
and rt = {
	rname: Sym.t;
	(* Mutable for creation only *)
	mutable properties: property array
}

and un = {
	uname: Sym.t;
	(* These must be TBool, TFloat, TInt, TVoid, or Rt *)
	mutable utypes: ty array
}

and parameter = Sym.t * ty
and ft = {
	fname: Sym.t;
	(* Mutable for creation only *)
	(*TODO: name just 'return'*)
	mutable return_type: ty;
	(* Mutable for creation only *)
	mutable parameters: parameter array
}

and ct_case = ty * ty
and ct = {
	cname: Sym.t;
	(* Mutable for creation only *)
	mutable ct_cases: ct_case array
}

type v =
	| Bool of bool
	| Float of float
	| Int of int
	| Void
	| Rc of rt * v array
	| Fn of fn
	| BuiltinFn of builtin_fn
	(*| Cn of cn*)
	| World

and builtin_fn = {
	builtin_ft: ft;
	(* Takes the 'pop' function *)
	exec: (unit -> v) -> v
}

and bytecode =
	| CallStatic of fn
	| CallBuiltin of builtin_fn
	| CallLambda
	| Case of (ty * int) array
	| Const of v
	| Construct of rt
	| Drop
	(* Load a value from `int` entries earlier in the stack *)
	| Load of int
	(* Goto-like codes store index of bytecode to move to. *)
	| Goto of int
	| GotoIfFalse of int
	| Return
	(* For `a = b; c`, we push `b`, then eval `c` (which may fetch `a`); then remove `a` from under it. *)
	| UnLet
	| Nil

and code = {
	bytecodes: bytecode array;
	locs: CodeLocs.t
}

and fn_type =
	| FnFt of ft
	| FnCt of ct

and fn = {
	fn_type: fn_type;
	(*TODO:rename to fn_mdl*)
	containing_modul: modul;
	(*TODO:rename to fn_code*)
	mutable code: code
}

(*
(*TODO: this is identical to fn except for having ct*Sym.t instead of ft*)
and cn = {
	mutable ct: ct;
	cn_name: Sym.t;
	cn_mdl: modul;
	(* This is the code for a case expression, so it always starts with a Case bytecode *)
	mutable cn_code: code
}
*)

(*TODO:rename to mdl*)
and modul = {
	file_name: string;
	values: v Sym.Lookup.t;
	types: ty Sym.Lookup.t;
}



(*TODO:MOVE!!!*)
let fn_type_as_ft(fn_type: fn_type): ft =
	match fn_type with
	| FnFt ft -> ft
	| _ -> assert false
let fn_type_as_ct(fn_type: fn_type): ct =
	match fn_type with
	| FnCt ct -> ct
	| _ -> assert false
