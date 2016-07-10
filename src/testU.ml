Printexc.record_backtrace true

let test_noze = Noze.create FileIO.file_system

let print_tokens(src: string): unit =
	let tokens = Noze.lex test_noze src in
	ArrayU.iter tokens begin fun (token, loc) ->
		OutputU.printf "%a @ %a\n" TokenU.output token Loc.output loc
	end

let lex(src: string): Token.t array =
	ArrayU.map (Noze.lex test_noze src) @@ fun (token, _) -> token

let parse(src: string): Ast.modul =
	Noze.parse test_noze src

let compile(src: string): N.modul =
	Noze.compile test_noze src

let time(f: unit -> 'a): 'a =
	let t1 = Sys.time() in
	let arr = MutArray.create() in
	for _ = 1 to 10000 do
		MutArray.add arr @@ f()
	done;
	let t2 = Sys.time() in
	Printf.printf "Execution time: %fs\n" @@ t2 -. t1;
	MutArray.get arr 0

let fn_named({N.values; _}: N.modul)(name: string): N.declared_fn =
	match Sym.Lookup.try_get values (Sym.of_string name) with
	| Some(N.Fn f) ->
		begin match f with
		| N.DeclaredFn f -> f
		| _ -> raise U.TODO
		end
	| _ -> failwith @@ OutputU.out_to_string "No function named \"%s\"" name

let call_fn(noze: Noze.t)(m: N.modul)(name: string)(vals: N.v array): N.v =
	let debug = false in
	let fn = fn_named m name in
	(if debug then Interpreter.debug_call_fn noze else Interpreter.call_fn) fn vals
