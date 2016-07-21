(*
let unix_file_descr: Unix.file_descr = Unix.openfile "test-nz/test.nz" [] 0
let l: Lwt_unix.file_descr =
	Lwt_unix.of_unix_file_descr ~blocking:false unix_file_descr
*)


(*let%lwt content = IoU.read @@ Path.of_string "test-nz/test.nz"
let () = OutputU.printf "%s\n" content*)


(*val read : file_descr -> bytes -> int -> int -> int

`read fd buff ofs len` reads len bytes from descriptor fd,
storing them in byte sequence buff,
starting at position ofs in buff.
Return the number of bytes actually read.*)


(*TODO: remember to use -no-debug with lwt ppx extension*)


let lwt_result: N.v Lwt.t =
	let%lwt modul = TestU.compile (Path.of_string "test-nz/test") in
	Lwt.return @@ TestU.call_fn TestU.test_noze modul "main" [| World.world |]
let result: N.v = Lwt_main.run lwt_result
let () = OutputU.printf "%a\n" ValU.output result

(* let () = OutputU.printf "%a\n" (ArrayU.output TokenU.output) @@ TestU.lex "test.nz" *)

(* let () = OutputU.printf "%a\n" AstU.output_modul @@ TestU.parse "test.nz" *)

(*let () =
	let _ = TestU.time @@ fun () ->
		TestU.parse "test.nz" in
	()*)
