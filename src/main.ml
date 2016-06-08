(* let src =
"fn decr Int x Int
	:one 1
	- x one

fn factorial Int x Int
	cond (< x 2) 1: * x: factorial: decr x
" *)

let src =
"rec Point
	x Int
	y Int

fn zero Point a Int
	Point a 2
"
(*
let compiler = TestU.test_compiler
let () =
	let _ = TestU.time begin fun () ->
		TestU.parse src
	end in
	()
*)

let modul = TestU.compile src
let result = Modul.call_fn modul (TestU.symbol "zero") [| Val.Int 1 |]
let () = OutputU.printf "%a\n" Val.output result

(* let x = BatFile.with_file_in "test.nz" use_file
let () = OutputU.printf "%s\n" x *)

(*
TODOS:

`func` -> `fn` in many places

use -safe-string

typ -> type_ast


*)
