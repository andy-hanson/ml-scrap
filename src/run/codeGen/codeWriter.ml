open N.V
open N.Ty
open N.TyP
open N.Code

(*TODO: ordering of functions could use some work*)

type code_idx = int
type stack_depth = int

type t = {
	bindings: Bind.t;
	type_of_ast: TypeOfAst.t;
	tys: TypeCheck.t;
	parameters: Ast.parameter array;

	mutable stack_depth: int;
	code: bytecode MutArray.t;
	locs_builder: CodeLocs.builder;
	local_depths: stack_depth AstLookup.LocalDeclare.t
}

let write_bc({code; locs_builder; _}: t)(loc: Loc.t)(bc: bytecode): unit =
	MutArray.push code bc;
	CodeLocs.write locs_builder loc

(*TODO:MOVE*)
let write(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(tys: TypeCheck.t)(parameters: Ast.parameter array)(loc: Loc.t)(do_write: t -> unit): code =
	let w: t =
		{
			bindings;
			type_of_ast;
			tys;
			parameters;

			stack_depth = 0;
			code = MutArray.create();
			locs_builder = CodeLocs.create_builder();
			local_depths = AstLookup.LocalDeclare.create()
		} in

	do_write w;

	Assert.equal w.stack_depth 1 OutputU.output_int;
	write_bc w loc Return;
	{bytecodes = MutArray.to_array w.code; locs = CodeLocs.finish w.locs_builder}

let apply_fn_to_stack_depth(w: t)(arity: int): unit =
	(* Take off args, push return value *)
	w.stack_depth <- w.stack_depth - arity + 1

let next_code_idx({code; _}: t): int =
	MutArray.length code

let set_local_depth({local_depths; stack_depth; _}: t)(declare: Ast.local_declare): unit =
	AstLookup.LocalDeclare.set local_depths declare stack_depth
let get_local_depth({local_depths; _}: t)(declare: Ast.local_declare): int =
	AstLookup.LocalDeclare.get local_depths declare
let incr_stack_depth(w: t): unit =
	w.stack_depth <- w.stack_depth + 1
let decr_stack_depth(w: t): unit =
	w.stack_depth <- w.stack_depth - 1

let set_code({code; _}: t)(code_idx: int)(bytecode: bytecode): unit =
	MutArray.set code code_idx bytecode

let access_local(w: t)(loc: Loc.t)(local: Ast.local_declare): unit =
	write_bc w loc @@ Load(get_local_depth w local);
	incr_stack_depth w

let access_parameter({parameters; _} as w: t)(loc: Loc.t)(parameter: Ast.parameter): unit =
	let param_index = OpU.force @@ ArrayU.find_index parameters @@ (==) parameter in
	write_bc w loc @@ Load(param_index - Array.length parameters);
	incr_stack_depth w

let bindings({bindings; _}: t): Bind.t =
	bindings
let type_of_ast({type_of_ast; _}: t): TypeOfAst.t =
	type_of_ast
let tys({tys; _}: t): TypeCheck.t =
	tys

let dup(w: t)(loc: Loc.t): unit =
	write_bc w loc Dup;
	incr_stack_depth w

let un_let(w: t)(loc: Loc.t)(n: int): unit =
	write_bc w loc @@ UnLet n;
	w.stack_depth <- w.stack_depth - n

let const(w: t)(loc: Loc.t)(value: v): unit =
	write_bc w loc @@ Const value;
	incr_stack_depth w

let drop(w: t)(loc: Loc.t): unit =
	write_bc w loc Drop;
	decr_stack_depth w

let call(w: t)(loc: Loc.t)(arity: int): unit =
	write_bc w loc Call;
	(* Decr one for the lambda itself *)
	decr_stack_depth w;
	apply_fn_to_stack_depth w arity

let partial(w: t)(loc: Loc.t)(arity: int): unit =
	write_bc w loc @@ Partial arity;
	(* Take 'arity' args off, and take the fn off, but push a partially applited function back. *)
	w.stack_depth <- w.stack_depth - arity

let quote(w: t)(loc: Loc.t)(strings: string array): unit =
	write_bc w loc @@ Quote strings;
	(* Pop n-1 args off the stack and push one back o *)
	w.stack_depth <- w.stack_depth - (Array.length strings - 2)

let check(w: t)(loc: Loc.t): unit =
	write_bc w loc Check
	(* No stack effect: pop bool, push void *)

let destruct(w: t)(loc: Loc.t)(patterns: pattern array): unit =
	(* Stack depth should have been handled by the caller...*)
	write_bc w loc @@ Destruct patterns

let get_property(w: t)(loc: Loc.t)(property_index: int): unit =
	write_bc w loc @@ GetProperty property_index
	(* No stack effect: pop record, push property *)

let cnv_rc(w: t)(loc: Loc.t)(rt: rt)(indexes: int array): unit =
	write_bc w loc @@ CnvRc(rt, indexes)
	(* No stack effect: pop record, push record *)

type placeholder = code_idx
let placeholder(w: t)(loc: Loc.t): placeholder =
	U.returning (next_code_idx w) @@ fun _ -> write_bc w loc Return
let resolve_goto_if_false(w: t)(p: placeholder): unit =
	set_code w p @@ GotoIfFalse(next_code_idx w)
let resolve_goto(w: t)(p: placeholder): unit =
	set_code w p @@ Goto(next_code_idx w)

type cases = (ty * int) array
let cs(w: t)(loc: Loc.t)(n_parts: int): cases =
	U.returning (Array.make n_parts (t_nil, -1)) @@ fun dummy_parts ->
		write_bc w loc @@ Cs dummy_parts
let resolve_cs_part(w: t)(cases: cases)(part_index: int)(ty: ty): unit =
	cases.(part_index) <- ty, next_code_idx w
