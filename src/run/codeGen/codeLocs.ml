type t = Loc.t array
let get = Array.get
let empty = [||]

type builder = Loc.t MutArray.t

let create_builder = MutArray.create
let write = MutArray.add
let finish = MutArray.to_array

let output(out: 'o OutputU.t)(locs: t): unit =
	(OutputU.out_array Loc.output) out locs
