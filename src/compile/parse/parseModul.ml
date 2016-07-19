let count(f: unit -> 'a option): int * 'a =
	U.loop 0 @@ fun loop n ->
		match f() with
		| Some a -> n, a
		| None -> loop @@ n + 1

let parse_import_path_and_first_import(l: Lexer.t)(start: Loc.pos)(next: Token.t): Ast.import_path * Ast.local_declare =
	let n_leading_dots, start_name =
		match next with
		| Token.Dot ->
			(* First '.' is special because it can be repeated to reach parents. *)
			let n_extra_dots, name = count begin fun () ->
				match Lexer.next l with
				| Token.Dot -> None
				| Token.Name n -> Some(n)
				| x -> ParseU.unexpected start l x
				end in
			(n_extra_dots + 1), name
		| Token.Name n ->
			0, n
		| x ->
			ParseU.unexpected start l x in

	let names, first_import =
		ArrayU.build_with_first start_name begin fun () ->
			let start, next = Lexer.pos_next l in
			match next with
			| Token.Dot -> ArrayU.Cont(ParseU.parse_name l)
			| Token.Name n -> ArrayU.Done(Lexer.loc_from l start, n)
			| x -> ParseU.unexpected start l x
		end in

	let path =
		match n_leading_dots with
		| 0 -> Ast.Global names
		| n -> Ast.Relative(n, names) in

	path, first_import

let parse_imports(l: Lexer.t): Ast.imports array =
	ArrayU.build_loop begin fun () ->
		let start, next = Lexer.pos_next l in
		let path, first_import = parse_import_path_and_first_import l start next in
		let imported_names, continue =
			ArrayU.build_with_first first_import begin fun () ->
				let start, next = Lexer.pos_next l in
				match next with
				| Token.Name n -> ArrayU.Cont(Lexer.loc_from l start, n)
				| Token.Newline -> ArrayU.Done(true)
				| Token.Dedent -> ArrayU.Done(false)
				| x -> ParseU.unexpected start l x
			end in
		let imports = Lexer.loc_from l start, path, imported_names in
		imports, continue
	end


let f(l: Lexer.t): Ast.modul =
	let start, next = Lexer.pos_next l in
	let imports, (start, next) =
		match next with
		| Token.Import ->
			ParseU.must_skip l Token.Indent;
			let imports = parse_imports l in
			imports, Lexer.pos_next l
		| _ ->
			[||], (start, next) in

	let decls = ArrayU.build_fold (start, next) begin fun (start, next) ->
		match next with
		| Token.EOF ->
			None, None
		| next ->
			let decl = ParseDecl.parse_decl l start next in
			Some(decl), Some(Lexer.pos_next l)
	end in

	imports, decls
