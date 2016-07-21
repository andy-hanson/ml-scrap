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

let v: FileIo.t = object
	method read(path: Path.t): string Lwt.t =
		(*TODO: wrap exceptions in FileIo.FileNotFound*)
			let path = Path.to_string path in
			(* This only matters if the file is created *)
			let permissions = 0 in
			let%lwt fd: Lu.file_descr Lwt.t = Lu.openfile path [Lu.O_RDONLY; Lu.O_NONBLOCK] permissions in
			read_all_bytes fd

	(*
	method open_in(path: Path.t): BatIO.input =
		try
			BatFile.open_in (Path.to_string path)
		with Sys_error _ ->
			raise @@ FileNotFound path

	method close_in: BatIO.input -> unit =
		BatIO.close_in
	*)
end
