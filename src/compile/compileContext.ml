type t = {
	symbol: string -> Symbol.t;
	keyword: Symbol.t -> Token.t option;
	emit_warning: CompileError.warning -> unit
}

let make(symbol: string -> Symbol.t)(keyword: Symbol.t -> Token.t option)(emit_warning: CompileError.warning -> unit): t =
	{ symbol; keyword; emit_warning }

let symbol ctx str =
	ctx.symbol str

let keyword ctx sym =
	ctx.keyword sym

let warn(ctx: t)(loc: Loc.t)(message: CompileError.message): unit =
	ctx.emit_warning (CompileError.Warning(loc, message))
