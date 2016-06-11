type t = { name: string }

let make(name: string): t =
	{ name = name }

let string_of({name}: t): string =
	name

let output(out: 'o OutputU.t)(symbol: t): unit =
	OutputU.str out symbol.name

let eq = (==)

type t' = t

module Map = MapU.Make(struct
	type t = t'
	let compare s1 s2 =
		if s1 == s2 then 0 else compare s1.name s2.name
end)

module Lookup = Lookup.Make(struct
	type t = t'
	(*TODO: memory-moving gc may ruin this*)
	let equal = (==)
	let hash s = Hashtbl.hash (string_of s)
end)
