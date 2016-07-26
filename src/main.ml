Printexc.record_backtrace true

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

let result: N.V.v =
	let modul = TestU.compile (Path.of_string "test-nz/test") in
	TestU.call_fn modul "main" [| World.world |]
let () = OutputU.printf "%a\n" ValOut.output result

(*
let () =
	let r = ref 0 in
	let fa () =
		for _ = 1 to 100 do
			incr r
		done in
	let threads = Array.init 100 @@ fun _ -> Thread.create fa () in
	Array.iter Thread.join threads;
	Printf.printf "%i" !r
*)


(* let () = OutputU.printf "%a\n" (ArrayU.output TokenU.output) @@ TestU.lex "test.nz" *)

(* let () = OutputU.printf "%a\n" AstOut.output_modul @@ TestU.parse "test.nz" *)

(*let () =
	let _ = TestU.time @@ fun () ->
		TestU.parse "test.nz" in
	()*)
