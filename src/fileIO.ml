type file_name = string

class type t = object
	method read: 'a. file_name -> (BatIO.input -> 'a) -> 'a
end

let file_system: t = object (_: t)
	method read file_name f =
		let input = BatFile.open_in file_name in
		U.returning (f input) begin fun _ ->
			BatIO.close_in input
		end
end
