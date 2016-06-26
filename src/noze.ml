type t = {
	io: FileIO.t
}

let create(io: FileIO.t): t =
	{io}

(* This reads the file again every time!*)
let translate_loc({io; _}: t)(file_name: FileIO.file_name)(loc: Loc.t): Loc.lc_loc =
	io#read file_name @@ fun source -> Loc.lc_loc source loc

let catch_errors = true

let do_work(noze: t)(file_name: FileIO.file_name)(f: CompileContext.t -> 'a): 'a =
	let emit_warning w = raise @@ CompileError.T w in
	let ctx = CompileContext.make emit_warning in
	if catch_errors then
		try
			f ctx
		with CompileError.T CompileError.Warning(loc, message) ->
			let lc_loc = translate_loc noze file_name loc in
			OutputU.printf "Compile error at %s %a:\n%a\n"
				file_name
				Loc.output_lc_loc lc_loc
				CompileErrorU.output_message message;
			exit 1
	else
		f ctx

let lex({io; _} as noze: t)(file_name: FileIO.file_name): (Token.t * Loc.t) array =
	do_work noze file_name @@ Compile.lex io file_name

let parse({io; _} as noze: t)(file_name: FileIO.file_name): Ast.modul =
	do_work noze file_name begin fun ctx ->
		io#read file_name @@ Parse.f ctx
	end

let compile({io; _} as noze: t)(file_name: FileIO.file_name): N.modul =
	do_work noze file_name @@ Compile.f io file_name

let lc_loc({io; _}: t)(file_name: FileIO.file_name)(loc: Loc.t): Loc.lc_loc =
	io#read file_name @@ fun source -> Loc.lc_loc source loc
