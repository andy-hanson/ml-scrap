Printexc.record_backtrace true

let test_noze = Noze.create FileIo.file_system

let print_tokens(path: Path.t): unit =
	let tokens = Noze.lex test_noze path in
	ArrayU.iter tokens @@ fun (token, loc) ->
		OutputU.printf "%a @ %a\n" TokenU.output token Loc.output loc

let lex(path: Path.t): Token.t array =
	ArrayU.map (Noze.lex test_noze path) @@ fun (token, _) -> token

let parse(path: Path.t): Ast.modul =
	Noze.parse test_noze path

let compile(path: Path.t): N.modul =
	Noze.compile test_noze path

let time(f: unit -> 'a): 'a =
	let t1 = Sys.time() in
	let arr = MutArray.create() in
	for _ = 1 to 10000 do
		MutArray.push arr @@ f()
	done;
	let t2 = Sys.time() in
	Printf.printf "Execution time: %fs\n" @@ t2 -. t1;
	MutArray.get arr 0

let val_named({N.vals; _}: N.modul)(name: string): N.v =
	OpU.or_else (Sym.Lookup.try_get vals (Sym.of_string name)) @@ fun () ->
		failwith @@ OutputU.out_to_string "No value named \"%s\"" name

let fn_named(modul: N.modul)(name: string): N.declared_fn =
	match val_named modul name with
	| N.Fn (N.DeclaredFn f) -> f
	| _ -> raise U.TODO

let call_fn(noze: Noze.t)(m: N.modul)(name: string)(vals: N.v array): N.v =
	let debug = true in
	let fn = fn_named m name in
	(if debug then Interpreter.debug_call_fn noze else Interpreter.call_fn) fn vals
