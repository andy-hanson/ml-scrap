(* let () = OutputU.printf "%a\n" (OutputU.out_array Token.output) (TestU.lex "test.nz") *)

(* let () = OutputU.printf "%a\n" AstU.output_modul (TestU.parse "test.nz") *)

let modul = TestU.compile "test.nz"
let result = TestU.call_fn TestU.test_noze modul
	(Sym.of_string "my-cnv")
	[| N.Int 1 (*N.World*) |]
let () = OutputU.printf "%a\n" ValU.output result

(*let () =
	let _ = TestU.time begin fun () ->
		TestU.parse "test.nz"
	end in
	()*)
