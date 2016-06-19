let call_fn
	(noze: Noze.t)
	({Val.info = {Val.ft = {Type.return_type; Type.parameters; _}; Val.containing_modul; _}; _} as fn: Val.fn)
	(args: Val.t array)
	: Val.t =
	ArrayU.iter_zip parameters args (fun (_, typ) arg -> Subsumes.check typ arg);
	let state = State.create fn in
	ArrayU.iter args (State.push state);
	let file_name = containing_modul.Val.file_name in
	let quiet = false in
	while not (if quiet then Step.step state else Step.debug_step noze file_name state) do () done;
	U.returning (State.peek state) (Subsumes.check return_type)
