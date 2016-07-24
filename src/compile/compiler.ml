open N

let create(io: FileIo.t): compiler =
	{io; moduls = Path.Lookup.create()}

(* This reads the file again every time!*)
let translate_loc({io; _}: compiler)(full_path: Path.t)(loc: Loc.t): Loc.lc_loc =
	FileIoU.read io full_path @@ fun source -> Loc.lc_loc source loc

let catch_errors = true

let err(compiler: compiler)(path: Path.t)((loc, message): Err.t): 'a =
	if path = [||] then
		OutputU.printf "Compile error:\n%a\n"
			OutputErr.output_message message
	else begin
		let lc_loc = translate_loc compiler path loc in
		OutputU.printf "Compile error at %a %a:\n%a\n"
			Path.output path
			Loc.output_lc_loc lc_loc
			OutputErr.output_message message
	end;
	(*TODO: better way of doing this*)
	exit 1

let do_work(compiler: compiler)(path: Path.t)(f: unit -> 'a): 'a =
	if catch_errors then
		try
			f()
		with Err.Exn error ->
			err compiler path error
	else
		f()

(*TODO: this is just for testing, so move*)
let lex({io; _} as compiler: compiler)(path: Path.t): (Token.t * Loc.t) array =
	do_work compiler path @@ fun () -> FileIoU.read io path Compile.lex

let parse({io; _} as compiler: compiler)(path: Path.t): Ast.modul =
	do_work compiler path @@ fun () -> FileIoU.read io path Parse.f

let compile({io; moduls} as compiler: compiler)(path: Path.t): modul =
	let get_modul: Path.t -> modul = Path.Lookup.get moduls in
	let err = {ModuleResolution.err = err compiler} in
	let linear_moduls = ModuleResolution.linearize_modul_dependencies io err path in
	ArrayU.iter linear_moduls begin fun (path, full_path, modul_ast) ->
		let get_modul_rel rel = get_modul @@ Path.resolve path rel in
		let modul = Compile.check_and_generate get_modul_rel path full_path modul_ast in
		Path.Lookup.set moduls path modul
	end;
	get_modul path

let lc_loc({io; _}: compiler)(path: Path.t)(loc: Loc.t): Loc.lc_loc =
	FileIoU.read io path @@ fun source -> Loc.lc_loc source loc
