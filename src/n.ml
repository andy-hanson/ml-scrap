type ty =
	| Any
	| TPrimitive of ty_primitive
	| Rt of rt
	| Un of un
	| TFn of ty_fn

and ty_fn =
	| Ft of ft
	| Ct of ct

and ty_primitive =
	| TBool
	| TFloat
	| TInt
	| TString
	| TVoid

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
(*TODO: this should store ft_mdl so fn doesn't have to*)
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
	| Primitive of primitive
	| Rc of rt * v array
	| Fn of fn

and primitive =
	| Bool of bool
	| Float of float
	| Int of int
	| String of string
	| Void

and bytecode =
	| Call
	| Cs of (ty * int) array
	| Const of v
	(*TODO:KILL | Construct of rt*)
	| Drop
	(* Load a value from `int` entries earlier in the stack *)
	| Load of int
	(* Goto-like codes store index of bytecode to move to. *)
	| Goto of int
	| GotoIfFalse of int
	| Return
	(* For `a = b; c`, we push `b`, then eval `c` (which may fetch `a`); then remove `a` from under it. *)
	| UnLet
	| Partial of int
	(* Pops n-1 arguments off the stack and interpolates them between the given strings. *)
	| Quote of string array
	| Check
	| Nil

and code = {
	bytecodes: bytecode array;
	locs: CodeLocs.t
}

and fn =
	| DeclaredFn of declared_fn
	| BuiltinFn of builtin_fn
	| PartialFn of partial_fn
	| Ctr of rt

(*TODO: we directly introspect this way too much. Create FnU.ml*)
and declared_fn = {
	(*TODO:RENAME*)
	fn_type: ty_fn;
	(*TODO:rename to fn_mdl*)
	containing_modul: modul;
	(*TODO:rename to fn_code*)
	mutable code: code
}

and partial_fn = {
	partially_applied: fn;
	partial_args: v array
}

and builtin_fn = {
	builtin_ty_fn: ty_fn;
	exec: interpreter_state -> unit
}

(*TODO:rename to mdl*)
and modul = {
	path: FileIO.path;
	values: v Sym.Lookup.t;
	types: ty Sym.Lookup.t;
}

(* These don't belong here, but ocaml hates recursion... *)
and call_stack_entry = {
	cse_fn: declared_fn;
	(* The fn's code *)
	cse_code: bytecode array;
	cse_locs: CodeLocs.t;
	(* Index of the first local on the stack. Parameters come before this. *)
	stack_start_index: int;
	(* Index where we were in this fn before entering another one *)
	mutable code_idx: int
}

and interpreter_state = {
	data_stack: v GoodStack.t;
	call_stack: call_stack_entry GoodStack.t;
	(* Currently executing fn. *Not* stored on stack. *)
	mutable cur: call_stack_entry;
}

let t_bool = TPrimitive TBool
let t_float = TPrimitive TFloat
let t_int = TPrimitive TInt
let t_string = TPrimitive TString
let t_void = TPrimitive TVoid

let v_bool b = Primitive(Bool b)
let v_float f = Primitive(Float f)
let v_int i = Primitive(Int i)
let v_string s = Primitive(String s)
let v_void = Primitive Void

type ty_or_v =
	| Ty of ty
	| V of v
