class type t = object
	inherit FileIo.t
	method add_file: Path.t -> string -> unit
end

let v(): t = object
	val files: string Path.Lookup.t = Path.Lookup.create()

	method add_file(path: Path.t)(content: string): unit =
		Path.Lookup.set files path content

	method read(path: Path.t): string Lwt.t =
		Lwt.return @@ try
			Path.Lookup.get files path
		with Not_found ->
			raise @@ FileIo.FileNotFound path
end
