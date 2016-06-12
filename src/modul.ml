type t = {
	fns: Code.fn array;
	recs: Type.rc array
}

let fn_named({fns; _}: t)(name: Symbol.t): Code.fn =
	try
		BatArray.find (fun f -> Symbol.eq name f.Code.fname) fns
	with Not_found ->
		failwith (OutputU.out_to_string "No function named %a" Symbol.output name)

let call_fn(m: t)(name: Symbol.t)(vals: Val.t array): Val.t =
	Interpret.call_fn (fn_named m name) vals
