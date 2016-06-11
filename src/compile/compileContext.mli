type t

val make: Symbols.t -> (CompileError.warning -> unit) -> t

val symbol: t -> string -> Symbol.t
val keyword: t -> Symbol.t -> Token.t option
val builtins_scope: t -> Scope.t
val warn: t -> Loc.t -> CompileError.message -> unit
