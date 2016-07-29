(* Any mutable properties in this file are mutable for *creation only*. *)

module rec Ty: sig
	type ty =
		| TPrimitive of ty_primitive (* Please never call this constructor unless you are the TyP module. *)
		| Rt of rt
		| Un of un
		| Ft of ft
		| GenRt of gen_rt
		| GenFt of gen_ft
		| GenUn of gen_un
		| GenVar of gen_var

	and gen_var = Ast.ty_param
	and 'a gen_stuff = {
		gen_params: gen_var array;
		gen_cache: 'a GenCache.t
	}

	and ty_primitive =
		| TBool
		| TFloat
		| TInt
		| TString
		| TVoid
		(* This is just a dummy type used during type creation. *)
		| TNil

	and rt_origin =
		| RtBuiltin of Sym.t
		(* This was directly declared. *)
		| RtDecl of Ast.rt
		(* This was expanded from a ty_inst *)
		| RtGenInst of gen_rt * ty array
	and property = Sym.t * ty
	and rt = {
		rt_origin: rt_origin;
		mutable properties: property array
	}
	and gen_rt = {
		gen_rt_origin: Ast.gen_rt;
		gen_rt_stuff: rt gen_stuff;
		mutable gen_rt_properties: property array
	}

	and un_origin =
		| UnBuiltin of Sym.t
		| UnDecl of Ast.un
		| UnGenInst of gen_un * ty array
	and un = {
		un_origin: un_origin;
		(* These must be TBool, TFloat, TInt, TVoid, or Rt *)
		(*TODO: type system should ensure this?*)
		(*TODO: darn tootin' type system should ensure this, and make sure we forbid anything else!*)
		mutable utys: ty array
	}
	and gen_un = {
		gen_un_origin: Ast.gen_un;
		gen_un_stuff: un gen_stuff;
		mutable gen_un_tys: ty array
	}

	and parameter = Sym.t * ty
	and ft_origin =
		| FtBuiltin of Sym.t
		| FtDecl of Ast.ft
		| FtFromFn of V.declared_fn
		| FtFromRt of rt
		| FtFromPartial of ft (* refers to an ft that this is a partial application of *)
		| FtGenInst of gen_ft * ty array
	and ft = {
		ft_origin: ft_origin;
		mutable return: ty;
		mutable parameters: parameter array
	}
	and gen_ft_origin =
		| GenFtDeclared of Ast.gen_ft
		| GenFtFromFn of V.declared_fn
	and gen_ft = {
		gen_ft_origin: gen_ft_origin;
		gen_ft_stuff: ft gen_stuff;
		mutable gen_ft_return: ty;
		mutable gen_ft_parameters: parameter array
	}

	type ft_or_gen =
	| FoG_Ft of ft
	| FoG_Gen of gen_ft
end = Ty

and TyP: sig
	val t_bool: Ty.ty
	val t_float: Ty.ty
	val t_int: Ty.ty
	val t_string: Ty.ty
	val t_void: Ty.ty
	val t_nil: Ty.ty
	val all_primitives: Ty.ty array
end = struct
	open Ty
	let t_bool = TPrimitive TBool
	let t_float = TPrimitive TFloat
	let t_int = TPrimitive TInt
	let t_string = TPrimitive TString
	let t_void = TPrimitive TVoid
	let t_nil = TPrimitive TNil
	(* Not including t_nil intentionally, since that's just an implementation detail. *)
	let all_primitives = [| t_bool; t_float; t_int; t_string; t_void |]
end

(*TODO:RENAME*)
and TyUU: sig
	val primitive_equal: Ty.ty_primitive -> Ty.ty_primitive -> bool
	val rt_equal: Ty.rt -> Ty.rt -> bool
	val gen_var_equal: Ty.gen_var -> Ty.gen_var -> bool
	val equal: Ty.ty -> Ty.ty -> bool
	val hash: Ty.ty -> int
end = struct
	open Ty
	let primitive_equal = (==)
	let rt_equal = (==)
	let gen_var_equal = (==)
	let equal(a: ty)(b: ty): bool =
		match a with
		| TPrimitive pa -> (match b with | TPrimitive pb -> primitive_equal pa pb | _ -> false)
		| Rt ra -> (match b with | Rt rb -> rt_equal ra rb | _ -> false)
		| Un ua -> (match b with | Un ub -> ua == ub | _ -> false)
		| Ft fa -> (match b with | Ft fb -> fa == fb | _ -> false)
		| GenRt ga -> (match b with | GenRt gb -> ga == gb | _ -> false)
		| GenUn ga -> (match b with | GenUn gb -> ga == gb | _ -> false)
		| GenFt ga -> (match b with | GenFt gb -> ga == gb | _ -> false)
		| GenVar ga -> (match b with | GenVar gb -> gen_var_equal ga gb | _ -> false)

	let rec hash: ty -> int = function
		| TPrimitive p ->
			Hashtbl.hash p
		| Rt {rt_origin; _} ->
			(*TODO: just use id?*)
			begin match rt_origin with
			| RtBuiltin name ->
				Sym.hash name
			| RtDecl(loc, _, _) ->
				Loc.hash loc
			| RtGenInst(gen, inst_with) ->
				HashU.combine (hash_gen_rt gen) @@ HashU.combine_n inst_with hash
			end
		| Un _ ->
			U.todo()
		| Ft _ ->
			U.todo()
		| GenVar(loc, _) ->
			Loc.hash loc
		| GenRt _ | GenFt _ | GenUn _ ->
			assert false
	and hash_gen_rt({gen_rt_origin; _}: gen_rt): int =
		let (loc, _, _, _) = gen_rt_origin in
		Loc.hash loc
end

(*TODO: rename to GenericCache*)
and GenCache: Lookup.S with type key = Ty.ty array = Lookup.Make(struct
	open Ty
	type t = ty array
	let equal: t -> t -> bool = ArrayU.eq TyUU.equal
	let hash(tys: t): int = HashU.combine_n tys TyUU.hash
end)

and V: sig
	type primitive =
		| Bool of bool
		| Float of float
		| Int of int
		| String of string
		| Void

	(*TODO: we directly introspect this way too much. Create FnU.ml*)
	(* Represents either an fn or cn *)
	type declared_fn = {
		fn_ast: Ast.fn;
		fn_ty: Ty.ft_or_gen;
		fn_modul: Compiler.modul;
		mutable fn_code: Code.code
	}

	type builtin_fn = {
		builtin_fn_ty: Ty.ft; (*TODO: ft_or_gen*)
		exec: Run.interpreter_state -> unit
	}

	type partial_fn = {
		partially_applied: fn;
		partial_args: v array
	}

	and fn =
		| DeclaredFn of declared_fn
		| BuiltinFn of builtin_fn
		| PartialFn of partial_fn
		| Ctr of Ty.rt

	and v =
		| Primitive of primitive
		| Rc of Ty.rt * v array
		| Fn of fn
end = V

and Compiler: sig
	type modul_member =
		| Ty of Ty.ty
		| V of V.v

	type modul = {
		(* Logical path, e.g. "a/b" *)
		path: Path.t;
		(* e.g. "a/b.nz" or "a/b/main.nz" *)
		full_path: Path.t;
		members: modul_member Sym.Lookup.t
	}

	type compiler = {
		io: FileIo.t;
		moduls: modul Path.Lookup.t
	}
end = Compiler

and Code: sig
	type pattern =
		| PSingle
		| PDestruct of pattern array

	type bytecode =
		| Call
		(*
		Converts to A from B.
		Parameters are A and a list of indexes into B to pull to A.
		(no need to store B
		*)
		| CnvRc of Ty.rt * int array
		| Cs of (Ty.ty * int) array
		| Const of V.v
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

	type code = {
		bytecodes: bytecode array;
		locs: CodeLocs.t
	}
end = Code

and Run: sig
	(* These don't belong here, but ocaml hates recursion... *)
	type call_stack_entry = {
		cse_fn: V.declared_fn;
		(* The fn's code *)
		cse_code: Code.bytecode array;
		cse_locs: CodeLocs.t;
		(* Index of the first local on the stack. Parameters come before this. *)
		stack_start_index: int;
		(* Index where we were in this fn before entering another one *)
		mutable code_idx: int
	}

	(*TODO: rename to stack_state*)
	type interpreter_state = {
		data_stack: V.v MutArray.t;
		call_stack: call_stack_entry MutArray.t;
		(* Currently executing fn. *Not* stored on stack. *)
		mutable cur: call_stack_entry;
	}

	type thread = {
		state: interpreter_state;
		mutable waiting_on: waiting_on;
		(* Threads to enqueue when this one completes. *)
		waited_on_by: thread MutArray.t
	}

	and waiting_on =
		| NotWaiting
		| Io of Thread.t
		| Thread of thread
		| Threads of thread MutArray.t

	type step_result =
		| NotDone
		| Done of V.v
		| AwaitingIo of Thread.t
		| AwaitingThread of thread

	type runtime = {
		mutable current_thread: thread option;
		(* All of these threads should have no `waiting_on`. *)
		thread_queue: thread Queue.t;
		(* These threads all have other `waiting_on` threads OR will be resumed by an Lwt after it completes. *)
		waiting_threads: thread MutArray.t;
		compiler: Compiler.compiler
	}
end = Run
