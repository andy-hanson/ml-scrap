type t = {
	ctx: CompileContext.t;
	lexer: Lexer.t
}

let make(ctx: CompileContext.t)(source: BatIO.input): t =
	{ ctx = ctx; lexer = Lexer.make source }

let next(p: t): Token.t =
	Lexer.next p.lexer p.ctx

let pos(p: t): Loc.pos =
	Lexer.pos p.lexer

let loc_from(p: t)(start: Loc.pos): Loc.t =
	Loc.make start (pos p)

let loc_at(p: t): Loc.t =
	Loc.single (pos p)
