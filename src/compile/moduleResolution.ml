type err = { err: 'a. Path.t -> Err.t -> 'a }

let extension = ".nz"
let main = Sym.of_string @@ "main" ^ extension

(* Returns the resolved path, with '.nz' included *)
let resolve({err}: err)(io: FileIo.t)(from: Path.t)(from_loc: Loc.t)(rel: Path.rel): (Path.t * string) Lwt.t =
	let base = Path.resolve from rel in

	(* Normally, just add the '.nz' extension. *)
	let pre, last = ArrayU.rtail base, ArrayU.last base in
	let regular_path = Array.append pre [| Sym.of_string @@ (Sym.string_of last) ^ extension |] in

	let attempt(path: Path.t)(on_fail: unit -> (Path.t * string) Lwt.t): (Path.t * string) Lwt.t =
		try%lwt
			let%lwt source = io#read path in
			Lwt.return (path, source)
		with FileIo.FileNotFound _ ->
			on_fail() in

	attempt regular_path @@ fun () ->
		let main_path = Array.append base [| main |] in
		attempt main_path @@ fun () ->
			err from (from_loc, Err.CantFindLocalModule(rel, regular_path, main_path))

module Moduls = Path.Lookup

(*
Loads modules and produces a linear compilation order.
https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
*)
(*TODO: passing in `err` as a parameter is ugly...*)
let linearize_modul_dependencies(io: FileIo.t)(err: err)(start_path: Path.t): (Path.t * Path.t * Ast.modul) array Lwt.t =
	(* This also functions as `visited` *)
	let map: ((Loc.t * Path.rel) array * Ast.modul) Moduls.t = Moduls.create() in

	LwtU.build @@ fun build ->
		U.loop3 [||] Loc.zero (0, start_path) @@ fun loop from from_loc rel ->
			let path = Path.resolve from rel in
			if Moduls.has_key map path then
				err.err from (from_loc, Err.CircularDependency path)
			else begin
				let%lwt full_path, source = resolve err io from from_loc rel in
				let (importses, _) as modul = Parse.f @@ BatIO.input_string source in

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
				let%lwt () = LwtU.join @@ ArrayU.map imports @@ fun (loc, rel) -> loop path loc rel in
				build (path, full_path, modul);
				Lwt.return_unit
			end
