type call_stack_entry = {
	fn: Val.fn;
	(* The fn's code *)
	code: Val.bytecode array;
	locs: CodeLocs.t;
	(* Index of the first local on the stack. Parameters come before this. *)
	stack_start_index: int;
	(* Index where we were in this fn before entering another one *)
	mutable code_idx: int
}
let entry_of({Val.info = {Val.code; _}; _} as fn: Val.fn)(cur_stack_index: int): call_stack_entry =
	{
		fn;
		code = code.Val.bytecodes;
		locs = code.Val.locs;
		stack_start_index = cur_stack_index;
		code_idx = 0
	}

type t = {
	data_stack: Val.t GoodStack.t;
	call_stack: call_stack_entry GoodStack.t;
	(* Currently executing fn. *Not* stored on stack. *)
	mutable cur: call_stack_entry;
}

let create(fn: Val.fn): t =
	{
		data_stack = GoodStack.create();
		call_stack = GoodStack.create();
		cur = entry_of fn (ValU.fn_arity fn)
	}

let peek({data_stack; _}: t): Val.t =
	GoodStack.peek data_stack

let pop({data_stack; _}: t): Val.t =
	GoodStack.pop data_stack

let pop_n({data_stack; _}: t)(n: int): Val.t array =
	GoodStack.pop_n data_stack n

let push({data_stack; _}: t)(value: Val.t): unit =
	GoodStack.push data_stack value

let un_let({data_stack; _}: t): unit =
	GoodStack.un_let data_stack

let cur_fn({cur = {fn; _}; _}: t): Val.fn =
	fn

let cur_code({cur = {code; code_idx; _}; _}: t): Val.bytecode =
	code.(code_idx)

let cur_loc({cur = {code_idx; locs; _}; _}: t): Loc.t =
	CodeLocs.get locs code_idx

let goto({cur; _}: t)(idx: int): unit =
	cur.code_idx <- idx
let goto_next({cur = {code_idx; _}; _} as state: t): unit =
	goto state (code_idx + 1)

let push_fn({call_stack; cur; data_stack; _} as state: t)(fn: Val.fn): unit =
	GoodStack.push call_stack cur;
	state.cur <- entry_of fn (GoodStack.size data_stack)

let pop_fn({call_stack; _} as state: t): bool =
	try
		state.cur <- GoodStack.pop call_stack;
		false
	with GoodStack.EmptyStack ->
		true

let load({cur = {stack_start_index; _}; data_stack; _}: t)(relative_index: int): Val.t =
	let index = stack_start_index + relative_index in
	GoodStack.get data_stack index

let assert_data_stack_back_to_function_start({cur = {stack_start_index; _}; data_stack; _}: t): unit =
	Assert.equal (GoodStack.size data_stack) stack_start_index OutputU.output_int

let debug_print
	(noze: Noze.t)
	(file_name: FileIO.file_name)
	({cur = {stack_start_index; _}; data_stack; _} as state: t): unit =
	OutputU.printf "Stack: %a (start: %d)\n"
		(GoodStack.output_with_max 3 ValU.output) data_stack
		stack_start_index;
	let lc_loc = Noze.lc_loc noze file_name (cur_loc state) in
	OutputU.printf "Executing: %a at %s:%a\n"
		ValU.output_bytecode (cur_code state)
		file_name
		Loc.output_lc_loc lc_loc
