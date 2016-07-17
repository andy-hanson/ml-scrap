type err = { err: 'a. Path.t -> Err.t -> 'a }

let extension = ".nz"
let main = Sym.of_string @@ "main" ^ extension

(* Returns the resolved path, with '.nz' included *)
let resolve({err}: err)(io: FileIo.t)(from: Path.t)(from_loc: Loc.t)(rel: Path.rel): Path.t * BatIO.input =
	let base = Path.resolve from rel in

	(* Normally, just add the '.nz' extension. *)
	let pre, last = ArrayU.rtail base, ArrayU.last base in
	let regular_path = Array.append pre [| Sym.of_string @@ (Sym.string_of last) ^ extension |] in

	let attempt(path: Path.t)(on_fail: unit -> Path.t * BatIO.input): Path.t * BatIO.input =
		try
			let input = io#open_in path in
			path, input
		with FileIo.FileNotFound _ ->
			on_fail() in

	attempt regular_path begin fun () ->
		let main_path = Array.append base [| main |] in
		attempt main_path begin fun () ->
			err from (from_loc, Err.CantFindLocalModule(rel, regular_path, main_path))
		end
	end


module Moduls = Path.Lookup

(*
Loads modules and produces a linear compilation order.
https://en.wikipedia.org/wiki/Topological_sorting#Depth-first_search
*)
(*TODO: passing in `err` as a parameter is ugly...*)
let linearize_modul_dependencies(io: FileIo.t)(err: err)(start_path: Path.t): (Path.t * Path.t * Ast.modul) array =
	(* This also functions as `visited` *)
	let map: ((Loc.t * Path.rel) array * Ast.modul) Moduls.t = Moduls.create() in

	ArrayU.build begin fun build ->
		let rec recur(from: Path.t)(from_loc: Loc.t)(rel: Path.rel): unit =
			let path = Path.resolve from rel in
			if Moduls.has_key map path then
				err.err from (from_loc, Err.CircularDependency path)
			else begin
				let full_path, input = resolve err io from from_loc rel in
				let (importses, _) as modul = Parse.f input in
				io#close_in input;

				(* Calculate dependencies *)
				let imports: (Loc.t * Path.rel) array =
					ArrayU.filter_map importses begin fun (loc, imported, _) ->
						match imported with
						| Ast.Global _ ->
							(*TODO: global module resolution*)
							(*For now, just assume it's a builtin.*)
							None
						| Ast.Relative r ->
							Some(loc, r)
					end in
				(* Mark this node as visited *)
				Moduls.set map path (imports, modul);
				ArrayU.iter imports begin fun (loc, rel) ->
					recur path loc rel
				end;
				build (path, full_path, modul)
			end in

		(*TODO: duplicate code*)
		recur [||] Loc.zero (0, start_path)
	end