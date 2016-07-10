open N

let entry_of({code; _} as cse_fn: declared_fn)(cur_stack_index: int): call_stack_entry =
	{
		cse_fn;
		cse_code = code.bytecodes;
		cse_locs = code.locs;
		stack_start_index = cur_stack_index;
		code_idx = 0
	}

let create(fn: declared_fn)(args: v array): interpreter_state =
	{
		data_stack = GoodStack.of_array args;
		call_stack = GoodStack.create();
		cur = entry_of fn @@ Array.length args
	}

let peek({data_stack; _}: interpreter_state): v =
	GoodStack.peek data_stack

let pop({data_stack; _}: interpreter_state): v =
	GoodStack.pop data_stack

let pop_n({data_stack; _}: interpreter_state)(n: int): v array =
	GoodStack.pop_n data_stack n

let push({data_stack; _}: interpreter_state)(value: v): unit =
	GoodStack.push data_stack value
let push_many({data_stack; _}: interpreter_state)(values: v array): unit =
	GoodStack.push_many data_stack values

let un_let({data_stack; _}: interpreter_state)(n: int): unit =
	GoodStack.un_let data_stack n

let cur_fn({cur = {cse_fn; _}; _}: interpreter_state): declared_fn =
	cse_fn

let cur_code({cur = {cse_code; code_idx; _}; _}: interpreter_state): bytecode =
	cse_code.(code_idx)

let cur_loc({cur = {code_idx; cse_locs; _}; _}: interpreter_state): Loc.t =
	CodeLocs.get cse_locs code_idx

let goto({cur; _}: interpreter_state)(idx: int): unit =
	cur.code_idx <- idx
let goto_next({cur = {code_idx; _}; _} as state: interpreter_state): unit =
	goto state @@ code_idx + 1

(*TODO: inline?*)
let push_fn({call_stack; cur; data_stack; _} as state: interpreter_state)(fn: declared_fn): unit =
	GoodStack.push call_stack cur;
	state.cur <- entry_of fn @@ GoodStack.size data_stack

let pop_fn({call_stack; _} as state: interpreter_state): bool =
	try
		state.cur <- GoodStack.pop call_stack;
		false
	with GoodStack.EmptyStack ->
		true

let load({cur = {stack_start_index; _}; data_stack; _}: interpreter_state)(relative_index: int): v =
	let index = stack_start_index + relative_index in
	GoodStack.get data_stack index

let call(state: interpreter_state)(called: v): unit =
	begin match called with
	| Primitive _ | Rc _ ->
		assert false
	| Fn f ->
		let rec call_fn = function
			| BuiltinFn {exec; _} ->
				exec state
			| DeclaredFn fn ->
				push_fn state fn
			| PartialFn {partially_applied; partial_args} ->
				push_many state partial_args;
				call_fn partially_applied
			| Ctr rt ->
				(*TODO: check property types*)
				let properties = pop_n state @@ TyU.rt_arity rt in
				push state @@ N.Rc(rt, properties) in
		call_fn f
	end

let assert_data_stack_back_to_function_start({cur = {stack_start_index; _}; data_stack; _}: interpreter_state): unit =
	Assert.equal (GoodStack.size data_stack) stack_start_index OutputU.output_int
