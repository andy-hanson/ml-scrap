open N

let get_export(loc: Loc.t)({vals; tys; _} as modul: N.modul)(name: Sym.t): ty_or_v =
	OpU.or_else (OpU.map (Sym.Lookup.try_get vals name) @@ fun v -> V v) @@ fun () ->
		OpU.or_else (OpU.map (Sym.Lookup.try_get tys name) @@ fun ty -> Ty ty) @@ fun () ->
			ErrU.raise loc @@ Err.ModuleHasNoMember(modul, name)
