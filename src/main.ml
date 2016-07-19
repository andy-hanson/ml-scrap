(*
(*
let unix_file_descr: Unix.file_descr = Unix.openfile "test-nz/test.nz" [] 0
let l: Lwt_unix.file_descr =
	Lwt_unix.of_unix_file_descr ~blocking:false unix_file_descr
*)

module Lu = Lwt_unix

let read_all_bytes(fd: Lu.file_descr): bytes Lwt.t =
	let len = 1024 in
	let bytes = Bytes.create len in
	let buffer = Buffer.create len in
	let rec recur(): unit Lwt.t =
		let%lwt n = Lu.read fd bytes 0 len in
		if n = len then begin
			Buffer.add_bytes buffer bytes;
			recur()
		end else begin
			Buffer.add_subbytes buffer bytes 0 n;
			Lwt.return ()
		end in
	recur() >>
	Lwt.return @@ Buffer.to_bytes buffer

let read_all_string(fd: Lu.file_descr): string Lwt.t =
	read_all_bytes fd


let%lwt f: Lu.file_descr Lwt.t = Lu.openfile "test-nz/test.nz" [Lu.O_RDONLY; Lu.O_NONBLOCK] 0
let%lwt content = read_all_string f

let () = OutputU.printf "%s\n" content
*)



(*val read : file_descr -> bytes -> int -> int -> int

`read fd buff ofs len` reads len bytes from descriptor fd,
storing them in byte sequence buff,
starting at position ofs in buff.
Return the number of bytes actually read.*)


(*TODO: remember to use -no-debug with lwt ppx extension*)


let modul = TestU.compile (Path.of_string "test-nz/test")
let result = TestU.call_fn TestU.test_noze modul "main" [| World.world |]
let () = OutputU.printf "%a\n" ValU.output result

(* let () = OutputU.printf "%a\n" (ArrayU.output TokenU.output) @@ TestU.lex "test.nz" *)

(* let () = OutputU.printf "%a\n" AstU.output_modul @@ TestU.parse "test.nz" *)

(*let () =
	let _ = TestU.time @@ fun () ->
		TestU.parse "test.nz" in
	()*)
