type t
val create: unit -> t
val get: t -> string -> Symbol.t
val keyword: t -> Symbol.t -> Token.t option
val builtins_scope: t -> Scope.t
