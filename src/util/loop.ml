let rec until_some(f: unit -> 'a option): 'a =
	match f() with
	| Some res ->
		res
	| None ->
		until_some f
