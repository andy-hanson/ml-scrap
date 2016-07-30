open Err

let raise_with_path(path: Path.t)(loc: Loc.t)(message: message): 'a =
	raise @@ CompileError { path; loc; message }

let raise(loc: Loc.t)(message: message): 'a =
	raise_with_path Path.empty loc message

let add_path(path: Path.t)(error: t): unit =
	assert (error.path == Path.empty);
	error.path <- path

let check(cond: bool)(loc: Loc.t)(message: message): unit =
	if not cond then
		raise loc message
