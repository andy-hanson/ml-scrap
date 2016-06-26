(*TODO: ordering of functions could use some work*)

type code_idx = int
type stack_depth = int

type t = {
	bindings: Bind.t;
	type_of_ast: TypeOfAst.t;
	types: TypeCheck.t;
	parameters: Ast.parameter array;

	mutable stack_depth: int;
	code: N.bytecode MutArray.t;
	locs: CodeLocs.builder;
	local_depths: stack_depth AstU.LocalDeclareLookup.t
}
let create(bindings: Bind.t)(type_of_ast: TypeOfAst.t)(types: TypeCheck.t)(parameters: Ast.parameter array): t = {
	bindings;
	type_of_ast;
	types;
	parameters;

	stack_depth = 0;
	code = MutArray.create();
	locs = CodeLocs.create_builder();
	local_depths = AstU.LocalDeclareLookup.create()
}

let apply_fn_to_stack_depth(w: t)(arity: int): unit =
	(* Take off args, push return value *)
	w.stack_depth <- w.stack_depth - arity + 1

let next_code_idx({code; _}: t): int =
	MutArray.length code

let write_bc({code; locs; _}: t)(loc: Loc.t)(bc: N.bytecode): unit =
	MutArray.add code bc;
	CodeLocs.write locs loc

let set_local_depth({local_depths; stack_depth; _}: t)(declare: Ast.local_declare): unit =
	AstU.LocalDeclareLookup.set local_depths declare stack_depth
let get_local_depth({local_depths; _}: t)(declare: Ast.local_declare): int =
	AstU.LocalDeclareLookup.get local_depths declare
let decr_stack_depth(w: t): unit =
	w.stack_depth <- w.stack_depth - 1
let incr_stack_depth(w: t): unit =
	w.stack_depth <- w.stack_depth + 1

let set_code({code; _}: t)(code_idx: int)(bytecode: N.bytecode): unit =
	MutArray.set code code_idx bytecode

(*TODO: rename to access_local*)
let write_local_access(w: t)(loc: Loc.t)(local: Ast.local_declare): unit =
	write_bc w loc @@ N.Load(get_local_depth w local);
	incr_stack_depth w

(*TODO: rename to access_parameter*)
let write_parameter_access({parameters; _} as w: t)(loc: Loc.t)(parameter: Ast.parameter): unit =
	(*TODO: Array.find_index helper*)
	let rec get_idx(i: int) = if parameters.(i) == parameter then i else get_idx (i + 1) in
	let param_index = get_idx 0 in
	write_bc w loc @@ N.Load(param_index - Array.length parameters);
	incr_stack_depth w

let finish({code; locs; stack_depth; _} as w: t)(loc: Loc.t): N.code =
	Assert.equal stack_depth 1 OutputU.output_int;
	write_bc w loc N.Return;
	{N.bytecodes = MutArray.to_array code; N.locs = CodeLocs.finish locs}

let binding({bindings; _}: t)(access: Ast.access): Binding.t =
	Bind.binding bindings access
let fn_of_ast({type_of_ast; _}: t)(fn_ast: Ast.fn): N.fn =
	TypeOfAst.fn_of_ast type_of_ast fn_ast
let rt_of_ast({type_of_ast; _}: t)(rt_ast: Ast.rt): N.rt =
	TypeOfAst.rt_of_ast type_of_ast rt_ast
let type_of_local({types; _}: t)(local: Ast.local_declare): N.ty =
	TypeCheck.type_of_local types local


let un_let(w: t)(loc: Loc.t): unit =
	write_bc w loc N.UnLet;
	decr_stack_depth w
let const(w: t)(loc: Loc.t)(value: N.v): unit =
	write_bc w loc @@ N.Const value;
	incr_stack_depth w
let drop(w: t)(loc: Loc.t): unit =
	write_bc w loc N.Drop;
	decr_stack_depth w
let call_builtin(w: t)(loc: Loc.t)(fn: N.builtin_fn)(arity: int): unit =
	write_bc w loc @@ N.CallBuiltin fn;
	apply_fn_to_stack_depth w arity
let call_static(w: t)(loc: Loc.t)(fn: N.fn)(arity: int): unit =
	write_bc w loc @@ N.CallStatic fn;
	apply_fn_to_stack_depth w arity
let construct(w: t)(loc: Loc.t)(rt: N.rt)(arity: int): unit =
	write_bc w loc @@ N.Construct rt;
	apply_fn_to_stack_depth w arity
let call_lambda(w: t)(loc: Loc.t)(arity: int): unit =
	write_bc w loc N.CallLambda;
	(* Decr one for the lambda itself *)
	decr_stack_depth w;
	apply_fn_to_stack_depth w arity


type placeholder = code_idx
let placeholder(w: t)(loc: Loc.t): placeholder =
	U.returning (next_code_idx w) @@ fun _ -> write_bc w loc N.Return
let resolve_goto_if_false(w: t)(p: placeholder): unit =
	set_code w p @@ N.GotoIfFalse(next_code_idx w)
let resolve_goto(w: t)(p: placeholder): unit =
	set_code w p @@ N.Goto(next_code_idx w)

type cases = (N.ty * int) array
let case(w: t)(loc: Loc.t)(n_parts: int): cases =
	U.returning (Array.make n_parts (N.TVoid, -1)) @@ fun dummy_parts -> write_bc w loc @@ N.Case dummy_parts
let resolve_case_part(w: t)(cases: cases)(part_index: int)(ty: N.ty): unit =
	cases.(part_index) <- ty, next_code_idx w
