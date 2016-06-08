type t = {
	io: FileIO.t;
	symbols: Symbols.t
}

let create(io: FileIO.t): t =
	{ io; symbols = Symbols.create() }

let catch_errors = true

let translate_loc({io; _}: t)(file_name: string)(loc: Loc.t): Loc.lc_loc =
	Loc.lc_loc (io#read file_name) loc

(*TODO: better name *)
(*TODO: maybe file_name should be in CompileContext.t*)
let do_work({symbols; _} as t: t)(file_name: string)(f: CompileContext.t -> 'a): 'a =
	let emit_warning w =
		(* For now, just raise it like an error. *)
		raise (CompileError.T w) in
	let ctx = CompileContext.make (Symbols.get symbols) (Symbols.keyword symbols) emit_warning in
	if catch_errors then
		try
			f ctx
		with CompileError.T CompileError.Warning(loc, message) ->
			let lc_loc = translate_loc t file_name loc in
			OutputU.printf "Compile error at %a:\n%a\n"
				Loc.output_lc_loc lc_loc
				CompileError.output_message message;
			exit 1;
			raise U.TODO (*TODO: this is unreachable...*)
	else
		f ctx

let lex({io; _} as t: t)(file_name: string): (Token.t * Loc.t) array =
	do_work t file_name begin fun ctx ->
		let source = io#read file_name in
		let lexer = Lexer.make ctx source in
		ArrayU.build_array_0 begin fun build ->
			let rec recur = fun () ->
				let start = Lexer.pos lexer in
				match Lexer.next lexer with
				| Token.End ->
					()
				| token ->
					build (token, Loc.make start (Lexer.pos lexer));
					recur() in
			recur()
		end
	end

let parse({io; _} as t: t)(file_name: string): Ast.modul =
	do_work t file_name begin fun ctx ->
		let source = io#read file_name in (*TODO: duplicate code*)
		Parse.f ctx source
	end

let compile({io; _} as t: t)(file_name: string): Modul.t =
	do_work t file_name begin fun ctx ->
		let source = io#read file_name in
		let modul = Parse.f ctx source in
		let bindings = Bind.bind ctx modul in
		let types = TypeCheck.f modul bindings in
		WriteCode.write_modul modul bindings types
	end

let symbol({symbols; _}: t)(string: string): Symbol.t =
	Symbols.get symbols string
