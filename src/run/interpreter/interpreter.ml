let call_fn
	(noze: Noze.t)
	({N.containing_modul; _} as fn: N.fn)
	(args: N.v array)
	: N.v =
	(*TODO: perform ct->ft transormation (join inputs, meet outputs)*)
	(*TODO: ArrayU.iter_zip parameters args (fun (_, typ) arg -> Subsumes.check typ arg);*)
	let state = State.create fn args in
	let file_name = containing_modul.N.file_name in
	let quiet = false in
	while not @@ if quiet then Step.step state else Step.debug_step noze file_name state do () done;
	State.peek state(*TODO: U.returning (State.peek state) @@ Subsumes.check return_type*)
