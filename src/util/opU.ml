let empty(op: 'a option): bool =
	match op with
	| Some _ ->
		false
	| None ->
		true

let force(op: 'a option): 'a =
	match op with
	| Some value ->
		value
	| None ->
		assert false

let op_if(cond: bool)(make: unit -> 'a): 'a option =
	if cond then
		Some(make())
	else
		None

let may(op: 'a option)(f: 'a -> unit): unit =
	match op with
	| Some value ->
		f value
	| None ->
		()

let map(op: 'a option)(f: 'a -> 'b): 'b option =
	match op with
	| Some value ->
		Some(f value)
	| None ->
		None

let or_else(op: 'a option)(default: unit -> 'a): 'a =
	match op with
	| Some value ->
		value
	| None ->
		default()

let or_try(op: 'a option)(or_else: unit -> 'a option): 'a option =
	match op with
	| Some _ ->
		op
	| None ->
		or_else()
