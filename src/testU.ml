Printexc.record_backtrace true

open N

let test_runtime = Runtime.create NativeFileIo.v
let test_compiler = test_runtime.compiler

let print_tokens(path: Path.t): unit =
	let tokens = Compiler.lex test_compiler path in
	ArrayU.iter tokens @@ fun (token, loc) ->
		OutputU.printf "%a @ %a\n" TokenU.output token Loc.output loc

let lex(path: Path.t): Token.t array =
	let tokens = Compiler.lex test_compiler path in
	ArrayU.map tokens @@ fun (token, _) -> token

let parse(path: Path.t): Ast.modul =
	Compiler.parse test_compiler path

let compile(path: Path.t): modul =
	Runtime.compile test_runtime path

let time(f: unit -> 'a): 'a =
	let t1 = Sys.time() in
	let arr = MutArray.create() in
	for _ = 1 to 10000 do
		MutArray.push arr @@ f()
	done;
	let t2 = Sys.time() in
	Printf.printf "Execution time: %fs\n" @@ t2 -. t1;
	MutArray.get arr 0


let fn_named(modul: modul)(name: string): declared_fn =
	match ModulU.get_export Loc.zero modul @@ Sym.of_string name with
	| V (Fn (DeclaredFn f)) -> f
	| _ -> failwith @@ "No function named " ^ name

let call_fn(modul: modul)(name: string)(args: v array): N.v =
	let fn = fn_named modul name in
	Runtime.add_thread test_runtime fn args;
	Runtime.run test_runtime


(*
let fn_named(modul: modul)(name: string): declared_fn =
	match ModulU.get_export Loc.zero modul @@ Sym.of_string name with
	| V (Fn (DeclaredFn f)) -> f
	| _ -> raise U.TODO

let call_fn(noze: Noze.t)(m: modul)(name: string)(vals: v array): N.v =
	let debug = true in
	let fn = fn_named m name in
	(if debug then Interpreter.debug_call_fn noze else Interpreter.call_fn) fn vals
*)
