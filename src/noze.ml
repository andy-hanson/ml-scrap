module Moduls = Path.Lookup

type t = {
	io: FileIo.t;
	moduls: N.modul Moduls.t
}

let create(io: FileIo.t): t =
	{io; moduls = Moduls.create()}

(* This reads the file again every time!*)
let translate_loc({io; _}: t)(full_path: Path.t)(loc: Loc.t): Loc.lc_loc Lwt.t =
	let%lwt source = io#read full_path in
	Lwt.return @@ Loc.lc_loc source loc

let catch_errors = true

let err(noze: t)(path: Path.t)((loc, message): Err.t): 'a =
	if path = [||] then
		OutputU.printf "Compile error:\n%a\n"
			OutputErr.output_message message
	else begin
		let lc_loc = Lwt_main.run @@ translate_loc noze path loc in
		OutputU.printf "Compile error at %a %a:\n%a\n"
			Path.output path
			Loc.output_lc_loc lc_loc
			OutputErr.output_message message
	end;
	(*TODO: better way of doing this*)
	exit 1

let do_work(noze: t)(path: Path.t)(f: unit -> 'a Lwt.t): 'a Lwt.t =
	if catch_errors then
		try%lwt
			f()
		with Err.Exn error ->
			err noze path error
	else
		f()

let lex({io; _} as noze: t)(path: Path.t): (Token.t * Loc.t) array Lwt.t =
	do_work noze path @@ fun () ->
		let%lwt source = io#read path in
		Lwt.return @@ Compile.lex source

let parse({io; _} as noze: t)(path: Path.t): Ast.modul Lwt.t =
	do_work noze path @@ fun () ->
		let%lwt source = io#read path in
		Lwt.return @@ Parse.f @@ BatIO.input_string source

let compile({io; moduls} as noze: t)(path: Path.t): N.modul Lwt.t =
	let err = {ModuleResolution.err = err noze} in
	let%lwt linear_moduls = ModuleResolution.linearize_modul_dependencies io err path in
	ArrayU.iter linear_moduls begin fun (path, full_path, modul_ast) ->
		let get_modul(rel: Path.rel): N.modul =
			Moduls.get moduls (Path.resolve path rel) in
		let modul = Compile.check_and_generate get_modul path full_path modul_ast in
		Moduls.set moduls path modul
	end;
	Lwt.return @@ Moduls.get moduls path

let lc_loc({io; _}: t)(path: Path.t)(loc: Loc.t): Loc.lc_loc Lwt.t =
	let%lwt source = io#read path in
	Lwt.return @@ Loc.lc_loc source loc
