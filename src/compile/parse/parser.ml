type t = {
	ctx: CompileContext.t;
	lexer: Lexer.t
}

let make(ctx: CompileContext.t)(source: string): t =
	{ ctx = ctx; lexer = Lexer.make source }

let next(p: t): Token.t =
	Lexer.next p.lexer p.ctx

let pos(p: t): Loc.pos =
	Lexer.pos p.lexer

let loc_from(p: t)(start: Loc.pos): Loc.t =
	Loc.make start (pos p)

let loc_at(p: t): Loc.t =
	Loc.single (pos p)

let get_restore(p: t): Lexer.restore =
	Lexer.get_restore p.lexer

let do_restore(p: t)(restore: Lexer.restore): unit =
	Lexer.do_restore p.lexer restore

let with_restore(p: t)(f: unit -> 'a): 'a =
	let restore = get_restore p in
	U.returning (f()) (fun _ -> do_restore p restore)
