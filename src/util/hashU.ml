(*TODO:MLI*)

let iter(tbl: ('k, 'v) Hashtbl.t)(f: ('k -> 'v -> unit)): unit =
	Hashtbl.iter f tbl

let build(builder: ('k -> 'v -> unit) -> unit): ('k, 'v) Hashtbl.t =
	(*TODO: can't currently use U.returning because of module resolution errors*)
	let tbl = Hashtbl.create 0 in
	builder (Hashtbl.add tbl);
	tbl
