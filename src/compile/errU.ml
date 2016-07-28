open Err

let raise(loc: Loc.t)(m: message): 'a =
	(*TODO: Properly get a backtrace and return that along with the exception.*)
	raise @@ Exn (loc, m)

let check(cond: bool)(loc: Loc.t)(message: message): unit =
	if not cond then
		raise loc message
