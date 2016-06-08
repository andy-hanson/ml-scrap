type t = { name: string }

let make(name: string): t =
	{ name = name }

let string_of({name}: t): string =
	name

let output(out: 'a OutputU.t)(symbol: t): unit =
	OutputU.str out symbol.name

let eq(a: t)(b: t): bool =
	(*TODO: memory-moving gc may ruin this*)
	a == b

(*TODO: Just use Lookup, and move MapU stuff to lookup!!!*)
module SymbolMapImpl = struct
	type tt = t
	type t = tt
	let compare s1 s2 =
		if s1 == s2 then 0 else compare s1.name s2.name
end

module SymMap = Map.Make(SymbolMapImpl)
