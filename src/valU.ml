open N.V
open N.Ty

let v_bool b = Primitive(Bool b)
let v_float f = Primitive(Float f)
let v_int i = Primitive(Int i)
let v_string s = Primitive(String s)
let v_void = Primitive Void

(* If this is raised, the typechecker was wrong. *)
(* TODO: kill and just assert false *)
exception CastFail

let equal(a: v)(b: v): bool =
	a = b

let bool_of = function
	| Primitive(Bool b) -> b
	| _ -> raise CastFail
let int_of = function
	| Primitive(Int i) -> i
	| _ -> raise CastFail
let float_of = function
	| Primitive(Float f) -> f
	| _ -> raise CastFail

let ty_of_primitive = function
	| Bool _ -> TBool
	| Int _ -> TInt
	| Float _ -> TFloat
	| String _ -> TString
	| Void -> TVoid

let rec ty_of_fn(fn: fn): ty =
	match fn with
	| BuiltinFn {builtin_fn_ty; _} ->
		Ft builtin_fn_ty
	| DeclaredFn {fn_ty; _} ->
		TyU.ty_of_ft_or_gen fn_ty
	| PartialFn {partially_applied; partial_args} ->
		let tp = match ty_of_fn partially_applied with | Ft f -> f | _ -> assert false in
		Ft(TyU.partial tp @@ Array.length partial_args)
	| Ctr _ ->
		U.todo() (*TODO: I left a ctr_type helper around somewhere...*)

and ty_of(v: v): ty =
 	match v with
	| Primitive p ->
		TPrimitive(ty_of_primitive p)
	| Fn fn ->
		ty_of_fn fn
	| Rc(rt, _) ->
		Rt rt

let fn_name({fn_ast; _}: declared_fn): Sym.t =
	AstU.fn_name fn_ast

let builtin_fn_name({builtin_fn_ty = {ft_origin; _}; _}: builtin_fn): Sym.t =
	match ft_origin with
	| FtBuiltin name -> name
	| _ -> assert false

let fn_arity({fn_ty; _}: declared_fn): int =
	TyU.ft_or_gen_arity fn_ty
