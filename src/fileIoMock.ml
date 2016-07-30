class type t = object
	inherit FileIo.t
	method add_file: Path.t -> string -> unit
end

let v(): t = object
	val files: string Path.Lookup.t = Path.Lookup.create()

	method add_file(path: Path.t)(content: string): unit =
		Path.Lookup.set files path content

	method open_in(path: Path.t): BatIO.input =
		let text =
			try
				Path.Lookup.get files path
			with Not_found ->
				raise @@ FileIo.FileNotFound path in
		BatIO.input_string text

	method close_in = ignore
end
