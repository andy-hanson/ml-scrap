let op_if(cond: bool)(make: unit -> 'a): 'a option =
	if cond then
		Some (make())
	else
		None

let may(op: 'a option)(f: 'a -> unit): unit =
	match op with
	| Some value ->
		f value
	| None ->
		()
