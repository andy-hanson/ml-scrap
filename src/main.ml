(* let () = OutputU.printf "%a\n" (OutputU.out_array Token.output) (TestU.lex "test.nz") *)

let modul = TestU.compile "test.nz"
(* let () = OutputU.printf "%a\n" AstU.output_modul modul_ast *)

let result = Modul.call_fn modul (TestU.symbol "main") [| Val.Float 1.5 |]
let () = OutputU.printf "%a\n" ValU.output result

(*let () =
	let _ = TestU.time begin fun () ->
		TestU.parse "test.nz"
	end in
	()*)
