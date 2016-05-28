type t = {
	fns: Code.func array;
	recs: Type.record array
}

let func_named(m: t)(name: Symbol.t): Code.func =
	try
		BatArray.find (fun f -> Symbol.eq f.Code.fname name) m.fns
	with Not_found ->
		failwith ("No function named " ^ name.Symbol.name)

let call_fn(m: t)(name: Symbol.t)(vals: Val.t array): Val.t =
	Interpret.call_fn (func_named m name) vals
