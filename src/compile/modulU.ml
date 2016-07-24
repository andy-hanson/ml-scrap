open N

let get_export(loc: Loc.t)({members; _} as modul: N.modul)(name: Sym.t): ty_or_v =
	OpU.or_else (Sym.Lookup.try_get members name) @@ fun () ->
		ErrU.raise loc @@ Err.ModuleHasNoMember(modul, name)
