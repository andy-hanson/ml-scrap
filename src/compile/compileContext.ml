type t = {
	syms: Symbols.t;
	warnings: CompileError.warning BatDynArray.t
}

let make(): t = {
	syms = Symbols.create();
	warnings = BatDynArray.create()
}

let symbol(ctx: t)(name: string): Symbol.t =
	Symbols.get ctx.syms name

let keyword(ctx: t)(name: Symbol.t): Token.t option =
	Symbols.keyword ctx.syms name

(* let symbols(ctx: t): Symbols.t =
	ctx.syms *)

let warn(ctx: t)(loc: Loc.t)(message: CompileError.message): unit =
	BatDynArray.add ctx.warnings (CompileError.Warning(loc, message))
