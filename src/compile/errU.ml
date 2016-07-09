open Err

let raise(loc: Loc.t)(m: message): 'a =
	raise @@ Exn (loc, m)

let check(cond: bool)(loc: Loc.t)(message: message): unit =
	if not cond then
		raise loc message
