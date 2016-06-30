
let debug_print(noze: Noze.t)(path: FileIO.path)
	({N.cur = {N.stack_start_index; _}; N.data_stack; _} as state: N.interpreter_state): unit =
	OutputU.printf "Stack: %a (start: %d)\n"
		(GoodStack.output_with_max 3 ValU.output) data_stack
		stack_start_index;
	let lc_loc = Noze.lc_loc noze path (State.cur_loc state) in
	OutputU.printf "Executing: %a at %s:%a\n"
		ValU.output_bytecode (State.cur_code state)
		path
		Loc.output_lc_loc lc_loc

let debug_step(noze: Noze.t)(path: FileIO.path)(state: N.interpreter_state): bool =
	debug_print noze path state;
	Step.step state


let call_fn_helper(fn: N.declared_fn)(args: N.v array)(step: N.interpreter_state -> bool): N.v =
	(*TODO: perform ct->ft transormation (join inputs, meet outputs)*)
	(*TODO: ArrayU.iter_zip parameters args (fun (_, typ) arg -> Subsumes.check typ arg);*)
	let state = State.create fn args in
	while not @@ step state do () done;
	State.peek state(*TODO: U.returning (State.peek state) @@ Subsumes.check return_type*)

let call_fn(fn: N.declared_fn)(args: N.v array): N.v =
	call_fn_helper fn args Step.step

let debug_call_fn(noze: Noze.t)(fn: N.declared_fn)(args: N.v array): N.v =
	call_fn_helper fn args @@ debug_step noze fn.N.containing_modul.N.path
