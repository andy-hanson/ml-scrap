Printexc.record_backtrace true

let test_noze = Noze.create NativeFileIo.v

let print_tokens(path: Path.t): unit Lwt.t =
	let%lwt tokens = Noze.lex test_noze path in
	Lwt.return @@ ArrayU.iter tokens @@ fun (token, loc) ->
		OutputU.printf "%a @ %a\n" TokenU.output token Loc.output loc

let lex(path: Path.t): Token.t array Lwt.t =
	let%lwt tokens = Noze.lex test_noze path in
	Lwt.return @@ ArrayU.map tokens @@ fun (token, _) -> token

let parse(path: Path.t): Ast.modul Lwt.t =
	Noze.parse test_noze path

let compile(path: Path.t): N.modul Lwt.t =
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

let fn_named(modul: N.modul)(name: string): N.declared_fn =
	match ModulU.get_export Loc.zero modul @@ Sym.of_string name with
	| N.V (N.Fn (N.DeclaredFn f)) -> f
	| _ -> raise U.TODO

let call_fn(noze: Noze.t)(m: N.modul)(name: string)(vals: N.v array): N.v =
	let debug = true in
	let fn = fn_named m name in
	(if debug then Interpreter.debug_call_fn noze else Interpreter.call_fn) fn vals
