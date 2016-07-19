exception FileNotFound of Path.t

class type t = object
	(* Throws FileNotFound *)
	method open_in: Path.t -> BatIO.input
	method close_in: BatIO.input -> unit
end

let file_system: t = object (_: t)
	method open_in(path: Path.t): BatIO.input =
		try
			BatFile.open_in (Path.to_string path)
		with Sys_error _ ->
			raise @@ FileNotFound path

	method close_in: BatIO.input -> unit =
		BatIO.close_in
end

(*TODO: move to TestU*)
class type mock = object
	inherit t
	method add_file: Path.t -> string -> unit
end
let mock(): mock = object (_: mock)
	val files: string Path.Lookup.t = Path.Lookup.create()

	method add_file(path: Path.t)(content: string): unit =
		Path.Lookup.set files path content

	method open_in(path: Path.t): BatIO.input =	
		let text =
			try
				Path.Lookup.get files path
			with Not_found ->
				raise @@ FileNotFound path in
		BatIO.input_string text

	method close_in(_: BatIO.input): unit =
		()
end
