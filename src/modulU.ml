let fn_named({Val.fns; _}: Val.modul)(name: Sym.t): Val.fn =
	let f = ArrayU.find fns (fun f -> Sym.eq name @@ ValU.fn_name f) in
	OpU.or_else f (fun () -> failwith (OutputU.out_to_string "No function named %a" Sym.output name))

let call_fn(noze: Noze.t)(m: Val.modul)(name: Sym.t)(vals: Val.t array): Val.t =
	Interpreter.call_fn noze (fn_named m name) vals
