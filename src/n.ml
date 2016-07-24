(*TODO: cleanup*)

(*
type origin =
	| Decl of Sym.t (*TODO: link to the Ast*)
	| Partial of ft * ty array
	| Inst of origin
*)

(*TODO: ty.ml*)
type ty =
	(*TODO: TPrimitive and Rt together are TConcrete. (They have no subtypes.) This is useful because unions can only contain concrete types.*)
	| TPrimitive of ty_primitive
	| Rt of rt
	| Un of un
	| Ft of ft
	(* Generics *)
	| TyGen of ty ty_gen
	| TyVar of ty_var
	| TyInst of ty ty_inst

(* ∀ *)
and 'a ty_gen = {
	ty_params: ty_var array;
	gen_ty: 'a
}

and ty_var = Ast.local_declare

and 'a ty_inst = {
	inst_gen: 'a ty_gen;
	inst_with: ty array
}

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
	(*TODO: type system should ensure this?*)
	(*TODO: darn tootin' type system should ensure this, and make sure we forbid anything else!*)
	mutable utys: ty array
}

and parameter = Sym.t * ty
and ft = {
	fname: Sym.t;
	(* Mutable for creation only *)
	mutable return: ty;
	(* Mutable for creation only *)
	mutable parameters: parameter array
}

(* V *)
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

and pattern =
	| PSingle
	| PDestruct of pattern array

and bytecode =
	| Call
	(*
	Converts to A from B.
	Parameters are A and a list of indexes into B to pull to A.
	(no need to store B
	*)
	| CnvRc of rt * int array
	| Cs of (ty * int) array
	| Const of v
	| Destruct of pattern array
	| Drop
	| Dup
	(* Stores index of property *)
	| GetProperty of int
	(* Load a value from `int` entries earlier in the stack *)
	| Load of int
	(* Goto-like codes store index of bytecode to move to. *)
	| Goto of int
	| GotoIfFalse of int
	| Return
	(* For `a = b; c`, we push `b`, then eval `c` (which may fetch `a`); then remove `a` from under it. *)
	| UnLet of int
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
(* Represents either an fn or cn *)
and declared_fn = {
	fn_ty: ft;
	fn_mdl: modul;
	mutable fn_code: code
}

and partial_fn = {
	partially_applied: fn;
	partial_args: v array
}

and builtin_fn = {
	builtin_fn_ty: ft;
	exec: interpreter_state -> unit
}



(*** COMPILER ***)

and compiler = {
	io: FileIo.t;
	moduls: modul Path.Lookup.t
}

and ty_or_v =
	| Ty of ty
	| V of v

(*TODO:rename to mdl*)
and modul = {
	(* Logical path, e.g. "a/b" *)
	path: Path.t;
	(* e.g. "a/b.nz" or "a/b/main.nz" *)
	full_path: Path.t;
	members: ty_or_v Sym.Lookup.t
}


(*** RUNTIME ***)


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

(*TODO: rename to stack_state*)
and interpreter_state = {
	data_stack: v MutArray.t;
	call_stack: call_stack_entry MutArray.t;
	(* Currently executing fn. *Not* stored on stack. *)
	mutable cur: call_stack_entry;
}

and step_result =
	| NotDone
	| Done of v
	| AwaitingIo of Thread.t
	| AwaitingThread of thread

and waiting_on =
	| NotWaiting
	| Io of Thread.t
	| Thread of thread
	| Threads of thread MutArray.t

and thread = {
	state: interpreter_state;
	mutable waiting_on: waiting_on;
	(* Threads to enqueue when this one completes. *)
	waited_on_by: thread MutArray.t
}

and runtime = {
	mutable current_thread: thread option;
	(* All of these threads should have no `waiting_on`. *)
	thread_queue: thread Queue.t;
	(* These threads all have other `waiting_on` threads OR will be resumed by an Lwt after it completes. *)
	waiting_threads: thread MutArray.t;
	compiler: compiler
}


(*TODO:TyU*)
let t_bool = TPrimitive TBool
let t_float = TPrimitive TFloat
let t_int = TPrimitive TInt
let t_string = TPrimitive TString
let t_void = TPrimitive TVoid

(*TODO:ValU*)
let v_bool b = Primitive(Bool b)
let v_float f = Primitive(Float f)
let v_int i = Primitive(Int i)
let v_string s = Primitive(String s)
let v_void = Primitive Void
