type t = {
	io: FileIO.t
}

let create(io: FileIO.t): t =
	{io}

(* This reads the file again every time!*)
let translate_loc({io; _}: t)(path: FileIO.path)(loc: Loc.t): Loc.lc_loc =
	io#read path @@ fun source -> Loc.lc_loc source loc

let catch_errors = true

let do_work(noze: t)(path: FileIO.path)(f: CompileContext.t -> 'a): 'a =
	let emit_warning w = raise @@ CompileError.Exn w in
	let ctx = CompileContext.make emit_warning in
	if catch_errors then
		try
			f ctx
		with CompileError.Exn(loc, message) ->
			let lc_loc = translate_loc noze path loc in
			OutputU.printf "Compile error at %s %a:\n%a\n"
				path
				Loc.output_lc_loc lc_loc
				CompileErrorU.output_message message;
			exit 1
	else
		f ctx

let lex({io; _} as noze: t)(path: FileIO.path): (Token.t * Loc.t) array =
	do_work noze path @@ Compile.lex io path

let parse({io; _} as noze: t)(path: FileIO.path): Ast.modul =
	do_work noze path begin fun ctx ->
		io#read path @@ Parse.f ctx
	end

let compile({io; _} as noze: t)(path: FileIO.path): N.modul =
	do_work noze path @@ Compile.f io path

let lc_loc({io; _}: t)(path: FileIO.path)(loc: Loc.t): Loc.lc_loc =
	io#read path @@ fun source -> Loc.lc_loc source loc
