let modul = TestU.compile (Path.of_string "test-nz/test")
let result = TestU.call_fn TestU.test_noze modul "main" [| World.world |]
let () = OutputU.printf "%a\n" ValU.output result


(* let () = OutputU.printf "%a\n" (ArrayU.output TokenU.output) @@ TestU.lex "test.nz" *)

(* let () = OutputU.printf "%a\n" AstU.output_modul @@ TestU.parse "test.nz" *)

(*let () =
	let _ = TestU.time begin fun () ->
		TestU.parse "test.nz"
	end in
	()*)
