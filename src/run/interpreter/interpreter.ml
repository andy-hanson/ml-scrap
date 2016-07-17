open N

let debug_print(noze: Noze.t)(full_path: Path.t)(state: interpreter_state): unit =
	let {cur = {stack_start_index; _}; data_stack; _} = state in
	OutputU.printf "Stack: %a (start_idx: %i)\n"
		(MutArray.output_with_max 3 ValU.output) data_stack
		stack_start_index;
	let lc_loc = Noze.lc_loc noze full_path @@ State.cur_loc state in
	OutputU.printf "Executing: %a at %a:%a\n"
		ValU.output_bytecode (State.cur_code state)
		Path.output full_path
		Loc.output_lc_loc lc_loc

let debug_step(noze: Noze.t)(path: Path.t)(state: interpreter_state): bool =
	debug_print noze path state;
	Step.step state

let call_fn_helper({fn_ty; _} as fn: declared_fn)(arguments: v array)(step: interpreter_state -> bool): v =
	Subsumes.check_fn_arguments fn_ty arguments;
	let state = State.create fn arguments in
	while not @@ step state do () done;
	U.returning (State.peek state) @@ Subsumes.check_fn_return fn_ty

let call_fn(fn: declared_fn)(args: v array): v =
	call_fn_helper fn args Step.step

let debug_call_fn(noze: Noze.t)(fn: declared_fn)(args: v array): v =
	call_fn_helper fn args @@ debug_step noze fn.fn_mdl.full_path
