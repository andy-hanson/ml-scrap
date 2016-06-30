type t = {
	emit_warning: CompileError.t -> unit
}

let make(emit_warning: CompileError.t -> unit): t =
	{emit_warning}

let warn(ctx: t)(loc: Loc.t)(message: CompileError.message): unit =
	ctx.emit_warning @@ (loc, message)
