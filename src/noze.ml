type t = {
	io: FileIO.t;
	symbols: Symbols.t
}

let create(io: FileIO.t): t =
	{ io; symbols = Symbols.create() }


(* This reads the file again every time!*)
let translate_loc({io; _}: t)(file_name: string)(loc: Loc.t): Loc.lc_loc =
	io#read file_name (fun source -> Loc.lc_loc source loc)

let catch_errors = true

let do_work({symbols; _} as noze: t)(file_name: string)(f: CompileContext.t -> 'a): 'a =
	let emit_warning w = raise (CompileError.T w) in
	let ctx = CompileContext.make symbols emit_warning in
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

let lex({io; _} as noze: t)(file_name: string): (Token.t * Loc.t) array =
	do_work noze file_name begin fun ctx ->
		io#read file_name begin fun source ->
			let l = Lexer.make ctx source in
			ArrayU.build begin fun build ->
				let rec recur = fun () ->
					let start, next = Lexer.pos_next l in
					match next with
					| Token.End ->
						()
					| token ->
						build (token, Lexer.loc_from l start);
						recur() in
				recur()
			end
		end
	end

let parse({io; _} as noze: t)(file_name: string): Ast.modul =
	do_work noze file_name begin fun ctx ->
		io#read file_name (Parse.f ctx)
	end

let compile({io; _} as noze: t)(file_name: string): Modul.t =
	do_work noze file_name begin fun ctx ->
		let modul = io#read file_name (Parse.f ctx) in
		let bindings = Bind.bind ctx modul in
		let types = TypeCheck.f modul bindings in
		CodeGen.f modul bindings types
	end

let symbol({symbols; _}: t)(string: string): Symbol.t =
	Symbols.get symbols string
