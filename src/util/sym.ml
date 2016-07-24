type t = {text: string}

let table: t Lookup.Str.t = Lookup.Str.create_with_size (1024 * 1024)

let of_string(text: string): t =
	Lookup.Str.get_or_update table text @@ fun () -> {text}

let of_buffer(buffer: BatBuffer.t): t =
	of_string @@ BatBuffer.contents buffer

let string_of({text}: t): string =
	text

let output(out: 'o OutputU.t)({text}: t): unit =
	OutputU.str out text

let eq = (==)
let hash(s: t) = Hashtbl.hash (string_of s)

type t' = t

module Map = MapU.Make(struct
	type t = t'
	let compare s1 s2 =
		if s1 == s2 then 0 else compare s1.text s2.text
end)

module Lookup = Lookup.Make(struct
	type t = t'
	let equal = (==)
	let hash = hash
end)
