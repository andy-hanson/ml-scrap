type t = Sym.t array
(* Parents, then a path from there *)
type rel = int * t

let empty = [||]

let to_string(path: t): string =
	OutputU.out_to_string "%a"
		(ArrayU.output_elements ~delimeter:"/" Sym.output)
		path

let of_string(s: string): t =
	let parts = Array.of_list @@ Str.split (Str.regexp "/") s in
	ArrayU.map parts Sym.of_string

let output(out: 'o OutputU.t)(path: t): unit =
	ArrayU.output Sym.output out path

let output_rel(out: 'o OutputU.t)((n_parents, rel_path): rel): unit =
	begin match n_parents with
	| 0 -> OutputU.str out "/"
	| 1 -> OutputU.str out "./"
	| n -> U.do_times (n - 1) (fun () -> OutputU.str out "../")
	end;
	output out rel_path

let resolve(path: t)((parents, rel_path) as rel: rel): t =
	if Array.length path < parents then
		failwith @@ OutputU.out_to_string "Can't resolve: %a, %a"
			output path
			output_rel rel;
	let remaining = Array.sub path 0 @@ Array.length path - parents in
	Array.append remaining rel_path

let add(path: t)(next: Sym.t): t =
	Array.append path [| next |]

let parent: t -> t = ArrayU.rtail

let add_extension(path: t)(extension: string): t =
	let l = ArrayU.last path in
	add (parent path) @@ Sym.of_string @@ Sym.string_of l ^ extension

type path = t
module Lookup = Lookup.Make(struct
	type t = path
	let equal: t -> t -> bool = ArrayU.eq Sym.eq
	let hash(path: t): int =
		Sym.hash @@ ArrayU.last path
end)
