open N.Compiler

let create(io: FileIo.t): compiler =
	{io; moduls = Path.Lookup.create()}

(* This reads the file again every time!*)
let translate_loc({io; _}: compiler)(full_path: Path.t)(loc: Loc.t): Loc.lc_loc =
	FileIoU.read io full_path @@ fun source -> Loc.lc_loc source loc


let output_error(compiler: compiler)(err: Err.t): unit =
	OutputErr.output (translate_loc compiler) BatIO.stderr err

let do_work(compiler: compiler)(path: Path.t)(f: unit -> 'a): 'a =
	try
		f()
	with (Err.CompileError error) as exn ->
		ErrU.add_path path error;
		output_error compiler error;
		raise exn

(*TODO:NAME*)
let do_work_2(compiler: compiler)(f: unit -> 'a): 'a =
	try
		f()
	with (Err.CompileError error) as exn ->
		output_error compiler error;
		raise exn

(*TODO: this is just for testing, so move*)
let lex({io; _} as compiler: compiler)(path: Path.t): (Token.t * Loc.t) array =
	do_work compiler path @@ fun () -> FileIoU.read io path Compile.lex

let parse({io; _} as compiler: compiler)(path: Path.t): Ast.modul =
	do_work compiler path @@ fun () -> FileIoU.read io path Parse.f

let compile({io; moduls} as compiler: compiler)(path: Path.t): modul =
	let get_modul: Path.t -> modul = Path.Lookup.get moduls in
	let linear_moduls =
		do_work_2 compiler @@ fun () ->
			ModuleResolution.linearize_modul_dependencies io path in
	ArrayU.iter linear_moduls begin fun (path, full_path, modul_ast) ->
		let get_modul_rel rel = get_modul @@ Path.resolve path rel in
		let modul = do_work compiler full_path @@ fun () ->
			Compile.check_and_generate get_modul_rel path full_path modul_ast in
		Path.Lookup.set moduls path modul
	end;
	get_modul path

let lc_loc({io; _}: compiler)(path: Path.t)(loc: Loc.t): Loc.lc_loc =
	FileIoU.read io path @@ fun source -> Loc.lc_loc source loc
