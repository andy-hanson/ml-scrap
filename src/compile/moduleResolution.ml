let extension = ".nz"
let main = Sym.of_string @@ "main" ^ extension

(* Returns the resolved path, with '.nz' included *)
let resolve(io: FileIo.t)(from: Path.t)(from_loc: Loc.t)(rel: Path.rel): Path.t * BatIO.input =
	let base = Path.resolve from rel in

	(* Normally, just add the '.nz' extension. *)
	let pre, last = ArrayU.rtail base, ArrayU.last base in
	let regular_path = Array.append pre [| Sym.of_string @@ (Sym.string_of last) ^ extension |] in

	let attempt(path: Path.t)(on_fail: unit -> Path.t * BatIO.input): Path.t * BatIO.input =
		try
			let source = io#open_in path in
			path, source
		with FileIo.FileNotFound _ ->
			on_fail() in

	attempt regular_path @@ fun () ->
		let main_path = Array.append base [| main |] in
		attempt main_path @@ fun () ->
			ErrU.raise_with_path from from_loc @@ Err.CantFindLocalModule(rel, regular_path, main_path)

module Moduls = Path.Lookup

(*
Loads modules and produces a linear compilation order.
https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
*)
let linearize_modul_dependencies(io: FileIo.t)(start_path: Path.t): (Path.t * Path.t * Ast.modul) array =
	(* This also functions as `visited` *)
	let map: ((Loc.t * Path.rel) array * Ast.modul) Moduls.t = Moduls.create() in

	ArrayU.build @@ fun build ->
		U.loop3 [||] Loc.zero (0, start_path) @@ fun loop from from_loc rel ->
			let path = Path.resolve from rel in
			if Moduls.has_key map path then
				ErrU.raise_with_path from from_loc @@ Err.CircularDependency path
			else begin
				let full_path, source = resolve io from from_loc rel in
				let (importses, _) as modul =
					try
						Parse.f source
					with (Err.CompileError error) as e ->
						ErrU.add_path full_path error;
						raise e in
				io#close_in source;

				(* Calculate dependencies *)
				let imports: (Loc.t * Path.rel) array =
					ArrayU.filter_map importses @@ fun (loc, imported, _) ->
						match imported with
						| Ast.Global _ ->
							(*TODO: global module resolution*)
							(*For now, just assume it's a builtin.*)
							None
						| Ast.Relative r ->
							Some(loc, r) in
				(* Mark this node as visited *)
				Moduls.set map path (imports, modul);
				let () = ThreadU.do_in_parallel imports @@ fun (loc, rel) -> loop path loc rel in
				(* We are linearizing, so can't write out this module until all of its dependencies are written. *)
				build (path, full_path, modul)
			end
