type t = {
	emit_warning: CompileError.warning -> unit
}

let make(emit_warning: CompileError.warning -> unit): t =
	{emit_warning}

let warn(ctx: t)(loc: Loc.t)(message: CompileError.message): unit =
	ctx.emit_warning (CompileError.Warning(loc, message))
