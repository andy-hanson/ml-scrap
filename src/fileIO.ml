type path = string

class type t = object
	method read: 'a. path -> (BatIO.input -> 'a) -> 'a
end

let file_system: t = object (_: t)
	method read path f =
		let input = BatFile.open_in path in
		U.returning (f input) begin fun _ ->
			BatIO.close_in input
		end
end

class type mock = object
	inherit t
	method add_file: path -> string -> unit
end
(*TODO: move to TestU*)
let mock(): mock = object (_: mock)
	val files: string Lookup.Str.t = Lookup.Str.create()

	method add_file(path: path)(content: string): unit =
		Lookup.Str.set files path content

	method read path f =
		f @@ BatIO.input_string @@ Lookup.Str.get files path
end
