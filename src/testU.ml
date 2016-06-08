Printexc.record_backtrace true

let myFileIO: FileIO.t = object
	method read(file_name: string): BatIO.input =
		(*cheat: file_name is actually the contents*)
		BatIO.input_string file_name
end

let test_compiler = Noze.create myFileIO

let print_tokens(src: string): unit =
	let tokens = Noze.lex test_compiler src in
	ArrayU.iter tokens begin fun (token, loc) ->
		OutputU.printf "%a @ %a\n" Token.output token Loc.output loc
	end

let parse(src: string): Ast.modul =
	Noze.parse test_compiler src

let compile(src: string): Modul.t =
	Noze.compile test_compiler src

let time(f: unit -> 'a): 'a =
	let t1 = Sys.time() in
	let arr = BatDynArray.create() in
	for _ = 1 to 10000 do
		let res = f() in
		BatDynArray.add arr res
	done;
	let t2 = Sys.time() in
	Printf.printf "Execution time: %fs\n" (t2 -. t1);
	BatDynArray.get arr 0

let symbol(s: string): Symbol.t =
	Noze.symbol test_compiler s
