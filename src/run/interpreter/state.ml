(*TODO: clean up this file*)

type call_stack_entry = {
	fn: N.fn;
	(* The fn's code *)
	code: N.bytecode array;
	locs: CodeLocs.t;
	(* Index of the first local on the stack. Parameters come before this. *)
	stack_start_index: int;
	(* Index where we were in this fn before entering another one *)
	mutable code_idx: int
}
let entry_of({N.code; _} as fn: N.fn)(cur_stack_index: int): call_stack_entry =
	{
		fn;
		code = code.N.bytecodes;
		locs = code.N.locs;
		stack_start_index = cur_stack_index;
		code_idx = 0
	}

type t = {
	data_stack: N.v GoodStack.t;
	call_stack: call_stack_entry GoodStack.t;
	(* Currently executing fn. *Not* stored on stack. *)
	mutable cur: call_stack_entry;
}

let create(fn: N.fn)(args: N.v array): t =
	{
		data_stack = GoodStack.of_array args;
		call_stack = GoodStack.create();
		cur = entry_of fn @@ Array.length args
	}

let peek({data_stack; _}: t): N.v =
	GoodStack.peek data_stack

let pop({data_stack; _}: t): N.v =
	GoodStack.pop data_stack

let pop_n({data_stack; _}: t)(n: int): N.v array =
	GoodStack.pop_n data_stack n

let push({data_stack; _}: t)(value: N.v): unit =
	GoodStack.push data_stack value

let un_let({data_stack; _}: t): unit =
	GoodStack.un_let data_stack

let cur_fn({cur = {fn; _}; _}: t): N.fn =
	fn

let cur_code({cur = {code; code_idx; _}; _}: t): N.bytecode =
	code.(code_idx)

let cur_loc({cur = {code_idx; locs; _}; _}: t): Loc.t =
	CodeLocs.get locs code_idx

let goto({cur; _}: t)(idx: int): unit =
	cur.code_idx <- idx
let goto_next({cur = {code_idx; _}; _} as state: t): unit =
	goto state @@ code_idx + 1

let push_fn({call_stack; cur; data_stack; _} as state: t)(fn: N.fn): unit =
	GoodStack.push call_stack cur;
	state.cur <- entry_of fn (GoodStack.size data_stack)

let pop_fn({call_stack; _} as state: t): bool =
	try
		state.cur <- GoodStack.pop call_stack;
		false
	with GoodStack.EmptyStack ->
		true

let load({cur = {stack_start_index; _}; data_stack; _}: t)(relative_index: int): N.v =
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
