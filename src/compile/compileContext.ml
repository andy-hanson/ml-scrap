type t = {
	symbols: Symbols.t;
	emit_warning: CompileError.warning -> unit
}

let make(symbols: Symbols.t)(emit_warning: CompileError.warning -> unit): t =
	{ symbols; emit_warning }

let symbol(ctx: t)(str: string): Symbol.t =
	Symbols.get ctx.symbols str

let keyword(ctx: t)(sym: Symbol.t): Token.t option =
	Symbols.keyword ctx.symbols sym

let builtins_scope(ctx: t): Scope.t =
	Symbols.builtins_scope ctx.symbols

let warn(ctx: t)(loc: Loc.t)(message: CompileError.message): unit =
	ctx.emit_warning (CompileError.Warning(loc, message))
