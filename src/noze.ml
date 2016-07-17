module Moduls = Path.Lookup

type t = {
	io: FileIo.t;
	moduls: N.modul Moduls.t
}

let create(io: FileIo.t): t =
	{io; moduls = Moduls.create()}

(* This reads the file again every time!*)
let translate_loc({io; _}: t)(full_path: Path.t)(loc: Loc.t): Loc.lc_loc =
	FileIoU.read io full_path @@ fun source -> Loc.lc_loc source loc

let catch_errors = true

let err(noze: t)(path: Path.t)((loc, message): Err.t): 'a =
	if path = [||] then
		OutputU.printf "Compile error:\n%a\n"
			OutputErr.output_message message
	else begin
		let lc_loc = translate_loc noze path loc in
		OutputU.printf "Compile error at %a %a:\n%a\n"
			Path.output path
			Loc.output_lc_loc lc_loc
			OutputErr.output_message message
	end;
	exit 1

let do_work(noze: t)(path: Path.t)(f: unit -> 'a): 'a =
	if catch_errors then
		try
			f()
		with Err.Exn error ->
			err noze path error
	else
		f()

let lex({io; _} as noze: t)(path: Path.t): (Token.t * Loc.t) array =
	do_work noze path @@ fun () -> Compile.lex io path

let parse({io; _} as noze: t)(path: Path.t): Ast.modul =
	do_work noze path @@ fun () -> FileIoU.read io path Parse.f

let compile({io; moduls} as noze: t)(path: Path.t): N.modul =
	let err = {ModuleResolution.err = err noze} in
	let linear_moduls = ModuleResolution.linearize_modul_dependencies io err path in
	ArrayU.iter linear_moduls begin fun (path, full_path, modul_ast) ->
		let get_modul(rel: Path.rel): N.modul =
			Moduls.get moduls (Path.resolve path rel) in
		let modul = Compile.check_and_generate get_modul path full_path modul_ast in
		Moduls.set moduls path modul
	end;
	Moduls.get moduls path

let lc_loc({io; _}: t)(path: Path.t)(loc: Loc.t): Loc.lc_loc =
	FileIoU.read io path @@ fun source -> Loc.lc_loc source loc
