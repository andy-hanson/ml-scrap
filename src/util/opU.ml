(*TODO:opU.mli*)

let may(op: 'a option)(f: 'a -> unit): unit =
	match op with
	| Some value ->
		f value
	| None ->
		()
