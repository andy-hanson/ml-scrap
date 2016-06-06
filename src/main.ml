Printexc.record_backtrace true

let print_tokens(source: BatIO.input): unit =
	let ctx = CompileContext.make() in
	let lexer = Lexer.make source in
	let break = ref false in
	while not !break do
		let token = Lexer.next lexer ctx in
		match token with
		| Token.End ->
			break := true
		| _ ->
			OutputU.printf "%a\n" Token.output token
	done

let get_only_fn(Ast.Modul(_, decls)): Ast.decl_val =
	match Array.get decls 0 with
	| Ast.Val fn -> fn
	| _ -> failwith "boo"

let print_ast(source: BatIO.input): unit =
	let ctx = CompileContext.make() in
	let modul = Parse.parse ctx source in
	OutputU.printf "%a\n" Ast.output_decl_val (get_only_fn modul)

let print_modul(m: Ast.modul): unit =
	OutputU.printf "%a\n" Ast.output_modul m

let print_fun(f: Ast.decl_val): unit =
	OutputU.printf "%a\n" Ast.output_decl_val f

(*TODO:RENAME*)
let do_stuff(ctx: CompileContext.t)(source: BatIO.input) =
	let modul = Parse.parse ctx source in
	let bindings = Bind.bind ctx modul in
	let types = TypeCheck.f ctx modul bindings in
	modul, bindings, types

let compile(ctx: CompileContext.t)(source: BatIO.input): Modul.t =
	let modul, bindings, types =
		if true then
			try
				do_stuff ctx source
			with CompileError.T (CompileError.Warning (loc, message)) ->
				OutputU.printf "Compile error at %a:\n%a\n" Loc.output loc CompileError.output_message message;
				exit 0
		else
			do_stuff ctx source in
	WriteCode.write_modul modul bindings types

let compile_and_run(source: BatIO.input)(fn_name: string)(params: Val.t array): unit =
	let ctx = CompileContext.make() in
	let m = compile ctx source in
	let f = Modul.func_named m (CompileContext.symbol ctx fn_name) in
	OutputU.printf "%a\n" Code.output f.Code.code;
	let res = Interpret.call_fn f params in
	OutputU.printf "%a\n" Val.output res

let print_code(f: Code.func): unit =
	OutputU.printf "%a\n" Code.output f.Code.code

let src =
"
fn decr Int x Int
	:one 1
	- x one

fn factorial Int x Int
	cond (< x 2) 1: * x: factorial: decr x
"

(*
TODOS:

Parser, lexer, reader don't need lookahead any more
	Convert reader to streaming (don't read_all)

`func` -> `fn` in many places

use -safe-string

typ -> type_ast


*)



(*
let src =
"rec Point
	x Int
	y Int

fn zero Point a Int
	Point a 2
" *)

(* let x = BatFile.with_file_in "test.nz" use_file
let () = OutputU.printf "%s\n" x *)

let ctx = CompileContext.make()
(* let () = print_tokens src *)
(* let () = print_ast src *)
(* let xxx = compile ctx (BatIO.input_string src) *)
(* let () = print_code (Modul.func_named xxx (CompileContext.symbol ctx "factorial")) *)

let _ = compile_and_run (BatIO.input_string src) "factorial" [| Val.Int 5 |]


(*TODO: atom: how to escape from find? *)
