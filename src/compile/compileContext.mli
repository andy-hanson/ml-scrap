type t

val make:
	(string -> Symbol.t) ->
	(Symbol.t -> Token.t option) ->
	(CompileError.warning -> unit) ->
	t

val symbol: t -> string -> Symbol.t

val keyword: t -> Symbol.t -> Token.t option

val warn: t -> Loc.t -> CompileError.message -> unit
