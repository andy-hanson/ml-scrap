let v: FileIo.t = object
	method open_in(path: Path.t): BatIO.input =
		try
			BatFile.open_in (Path.to_string path)
		with Sys_error _ ->
			raise @@ FileIo.FileNotFound path

	method close_in: BatIO.input -> unit =
		BatIO.close_in
end
