type t = {
	io: FileIO.t
}

let create(io: FileIO.t): t =
	{io}

(* This reads the file again every time!*)
let translate_loc({io; _}: t)(path: FileIO.path)(loc: Loc.t): Loc.lc_loc =
	io#read path @@ fun source -> Loc.lc_loc source loc

let catch_errors = true

let do_work(noze: t)(path: FileIO.path)(f: unit -> 'a): 'a =
	if catch_errors then
		try
			f()
		with Err.Exn(loc, message) ->
			let lc_loc = translate_loc noze path loc in
			OutputU.printf "Compile error at %s %a:\n%a\n"
				path
				Loc.output_lc_loc lc_loc
				OutputErr.output_message message;
			exit 1
	else
		f()

let lex({io; _} as noze: t)(path: FileIO.path): (Token.t * Loc.t) array =
	do_work noze path @@ fun () -> Compile.lex io path

let parse({io; _} as noze: t)(path: FileIO.path): Ast.modul =
	do_work noze path @@ fun () -> io#read path Parse.f

let compile({io; _} as noze: t)(path: FileIO.path): N.modul =
	do_work noze path @@ fun () -> Compile.f io path

let lc_loc({io; _}: t)(path: FileIO.path)(loc: Loc.t): Loc.lc_loc =
	io#read path @@ fun source -> Loc.lc_loc source loc
