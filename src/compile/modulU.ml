open N.Compiler

let get_export(loc: Loc.t)({members; _} as modul: modul)(name: Sym.t): modul_member =
	OpU.or_else (Sym.Lookup.try_get members name) @@ fun () ->
		ErrU.raise loc @@ Err.ModuleHasNoMember(modul, name)
