(* let src =
"fn decr Int x Int
	:one 1
	- x one

fn factorial Int x Int
	cond (< x 2) 1: * x: factorial: decr x
" *)

(*let () =
	let _ = TestU.time begin fun () ->
		TestU.parse "test.nz"
	end in
	()*)

let modul = TestU.compile "test.nz"
let result = Modul.call_fn modul (TestU.symbol "zero") [| Val.Int 1 |]
let () = OutputU.printf "%a\n" Val.output result


(* let x = BatFile.with_file_in "test.nz" use_file
let () = OutputU.printf "%s\n" x *)

(*
TODOS:

Use arr.(idx) instead of Array.get arr idx and arr.(idx) <- value instead of Array.set arr idx value

`func` -> `fn` in many places

use -safe-string

typ -> type_ast


*)
