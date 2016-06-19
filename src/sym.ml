type t = {name: string}

module Table = Lookup.Make(struct
	include String
	let hash = Hashtbl.hash
end)

let table: t Table.t = Table.create_with_size (1024 * 1024)

let of_string(name: string): t =
	Table.get_or_update table name (fun () -> {name})

let string_of({name}: t): string =
	name

let output(out: 'o OutputU.t)({name}: t): unit =
	OutputU.str out name

let eq = (==)

type t' = t

module Map = MapU.Make(struct
	type t = t'
	let compare s1 s2 =
		if s1 == s2 then 0 else compare s1.name s2.name
end)

module Lookup = Lookup.Make(struct
	type t = t'
	let equal = (==)
	let hash s = Hashtbl.hash (string_of s)
end)
