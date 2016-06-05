Printexc.record_backtrace true

let print_tokens(source: string): unit =
	let ctx = CompileContext.make() in
	let lexer = Lexer.make source in
	let break = ref false in
	while not !break do
		let token = Lexer.next lexer ctx in
		match token with
		| Token.End ->
			break := true
		| _ ->
			Batteries.Printf.printf "%a\n" Token.output token
	done

let get_only_fn(Ast.Modul(_, decls)): Ast.decl_val =
	match Array.get decls 0 with
	| Ast.Val fn -> fn
	| _ -> failwith "boo"

let print_ast(source: string): unit =
	let ctx = CompileContext.make() in
	let modul = Parse.parse ctx source in
	Batteries.Printf.printf "%a\n" Ast.output_decl_val (get_only_fn modul)

let print_modul(m: Ast.modul): unit =
	Batteries.Printf.printf "%a\n" Ast.output_modul m

let print_fun(f: Ast.decl_val): unit =
	Batteries.Printf.printf "%a\n" Ast.output_decl_val f

let do_stuff source ctx =
	let modul = Parse.parse ctx source in
	let bindings = Bind.bind ctx modul in
	let types = TypeCheck.f ctx modul bindings in
	modul, bindings, types

let compile(ctx: CompileContext.t)(source: string): Modul.t =
	let modul, bindings, types =
		if true then
			try
				do_stuff source ctx
			with CompileError.T (CompileError.Warning (loc, message)) ->
				Batteries.Printf.printf "Compile error at %a:\n%a\n" Loc.output loc CompileError.output_message message;
				exit 0
		else
			do_stuff source ctx in
	(* print_fun (get_only_fn modul); *)
	WriteCode.write_modul modul bindings types

let compile_and_run(source: string)(fn_name: string)(params: Val.t array): unit =
	let ctx = CompileContext.make() in
	let m = compile ctx source in
	let f = Modul.func_named m (CompileContext.symbol ctx fn_name) in
	Batteries.Printf.printf "%a\n" Code.output f.Code.code;
	let res = Interpret.call_fn f params in
	Batteries.Printf.printf "%a\n" Val.output res

let print_code(f: Code.func): unit =
	Batteries.Printf.printf "%a\n" Code.output f.Code.code

let src =
"fn decr Int x Int
	one Int = 1
	- x one

fn factorial Int x Int
	cond (< x 2) 1: * x: factorial: decr x
"

let src =
"rec Point
	x Int
	y Int

fn zero Point a Int
	Point a a
"

(*
TODO:
replace Array.iter with U.iter (reverse args)
atom: how to escape from find?

*)


let ctx = CompileContext.make()
(* let () = print_tokens src *)
(* let () = print_ast src *)
let xxx = compile ctx src
(* let () = print_code (Modul.func_named xxx (CompileContext.symbol ctx "factorial")) *)
let _ = compile_and_run src "zero" [| Val.Int 5 |]
