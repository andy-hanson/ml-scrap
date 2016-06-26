let eq = (=)

let join(loc: Loc.t)(types: N.ty array): N.ty =
	(*TODO actual join algorithm*)
	let t = Array.get types 0 in
	ArrayU.iter types begin fun typ ->
		CompileErrorU.check (t = typ) loc @@ CompileError.CombineTypes(t, typ)
	end;
	t
