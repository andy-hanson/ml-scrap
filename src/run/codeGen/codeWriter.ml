(*TODO: ordering of functions could use some work*)

type code_idx = int
type stack_depth = int

type t = {
	bindings: Bind.t;
	type_of_ast: TypeOfAst.t;
	tys: TypeCheck.t;
	parameters: Ast.parameter array;

	mutable stack_depth: int;
	code: N.bytecode MutArray.t;
	locs: CodeLocs.builder;
	local_depths: stack_depth AstU.LocalDeclareLookup.t
}

let write_bc({code; locs; _}: t)(loc: Loc.t)(bc: N.bytecode): unit =
	MutArray.push code bc;
	CodeLocs.write locs loc

(*TODO:MOVE*)
let write(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(tys: TypeCheck.t)(parameters: Ast.parameter array)(loc: Loc.t)(do_write: t -> unit): N.code =
	let w: t =
		{
			bindings;
			type_of_ast;
			tys;
			parameters;

			stack_depth = 0;
			code = MutArray.create();
			locs = CodeLocs.create_builder();
			local_depths = AstU.LocalDeclareLookup.create()
		} in

	do_write w;

	Assert.equal w.stack_depth 1 OutputU.output_int;
	write_bc w loc N.Return;
	{N.bytecodes = MutArray.to_array w.code; N.locs = CodeLocs.finish w.locs}

let apply_fn_to_stack_depth(w: t)(arity: int): unit =
	(* Take off args, push return value *)
	w.stack_depth <- w.stack_depth - arity + 1

let next_code_idx({code; _}: t): int =
	MutArray.length code

let set_local_depth({local_depths; stack_depth; _}: t)(declare: Ast.local_declare): unit =
	AstU.LocalDeclareLookup.set local_depths declare stack_depth
let get_local_depth({local_depths; _}: t)(declare: Ast.local_declare): int =
	AstU.LocalDeclareLookup.get local_depths declare
let incr_stack_depth(w: t): unit =
	w.stack_depth <- w.stack_depth + 1
let decr_stack_depth(w: t): unit =
	w.stack_depth <- w.stack_depth - 1

let set_code({code; _}: t)(code_idx: int)(bytecode: N.bytecode): unit =
	MutArray.set code code_idx bytecode

let access_local(w: t)(loc: Loc.t)(local: Ast.local_declare): unit =
	write_bc w loc @@ N.Load(get_local_depth w local);
	incr_stack_depth w

let access_parameter({parameters; _} as w: t)(loc: Loc.t)(parameter: Ast.parameter): unit =
	let param_index = OpU.force @@ ArrayU.find_index parameters @@ (==) parameter in
	write_bc w loc @@ N.Load(param_index - Array.length parameters);
	incr_stack_depth w

let bindings({bindings; _}: t): Bind.t =
	bindings
let type_of_ast({type_of_ast; _}: t): TypeOfAst.t =
	type_of_ast
let tys({tys; _}: t): TypeCheck.t =
	tys

let dup(w: t)(loc: Loc.t): unit =
	write_bc w loc N.Dup;
	incr_stack_depth w

let un_let(w: t)(loc: Loc.t)(n: int): unit =
	write_bc w loc @@ N.UnLet n;
	w.stack_depth <- w.stack_depth - n

let const(w: t)(loc: Loc.t)(value: N.v): unit =
	write_bc w loc @@ N.Const value;
	incr_stack_depth w

let drop(w: t)(loc: Loc.t): unit =
	write_bc w loc N.Drop;
	decr_stack_depth w

let call(w: t)(loc: Loc.t)(arity: int): unit =
	write_bc w loc N.Call;
	(* Decr one for the lambda itself *)
	decr_stack_depth w;
	apply_fn_to_stack_depth w arity

let partial(w: t)(loc: Loc.t)(arity: int): unit =
	write_bc w loc @@ N.Partial arity;
	(* Take 'arity' args off, and take the fn off, but push a partially applited function back. *)
	w.stack_depth <- w.stack_depth - arity

let quote(w: t)(loc: Loc.t)(strings: string array): unit =
	write_bc w loc @@ N.Quote strings;
	(* Pop n-1 args off the stack and push one back on. *)
	w.stack_depth <- w.stack_depth - (Array.length strings - 2)

let check(w: t)(loc: Loc.t): unit =
	write_bc w loc N.Check
	(* No stack effect: pop bool, push void *)

let destruct(w: t)(loc: Loc.t)(patterns: N.pattern array): unit =
	(* Stack depth should have been handled by the caller...*)
	write_bc w loc @@ N.Destruct patterns

let get_property(w: t)(loc: Loc.t)(property_index: int): unit =
	write_bc w loc @@ N.GetProperty property_index
	(* No stack effect: pop record, push property *)

let cnv_rc(w: t)(loc: Loc.t)(rt: N.rt)(indexes: int array): unit =
	write_bc w loc @@ N.CnvRc(rt, indexes)
	(* No stack effect: pop record, push record *)

type placeholder = code_idx
let placeholder(w: t)(loc: Loc.t): placeholder =
	U.returning (next_code_idx w) @@ fun _ -> write_bc w loc N.Return
let resolve_goto_if_false(w: t)(p: placeholder): unit =
	set_code w p @@ N.GotoIfFalse(next_code_idx w)
let resolve_goto(w: t)(p: placeholder): unit =
	set_code w p @@ N.Goto(next_code_idx w)

type cases = (N.ty * int) array
let cs(w: t)(loc: Loc.t)(n_parts: int): cases =
	U.returning (Array.make n_parts (N.t_void, -1)) @@ fun dummy_parts ->
		write_bc w loc @@ N.Cs dummy_parts
let resolve_cs_part(w: t)(cases: cases)(part_index: int)(ty: N.ty): unit =
	cases.(part_index) <- ty, next_code_idx w
