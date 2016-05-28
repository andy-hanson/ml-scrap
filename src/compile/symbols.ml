(*TODO: merge into CompileContext?*)

type table = (string, Symbol.t) Hashtbl.t

let create_table(): table = Hashtbl.create 4096

let t_get(t: table)(s: string): Symbol.t =
	try
		Hashtbl.find t s
	with Not_found ->
		(*TODO:helper code for cache pattern *)
		U.returning (Symbol.make s) (Hashtbl.add t s)

let create_name_to_keyword(table: table): (Symbol.t, Token.t) Hashtbl.t =
	let nk = Hashtbl.create 1 in
	let add keyword =
		let sym = t_get table (Token.keyword_to_string keyword) in
		Hashtbl.add nk sym keyword in
	Array.iter add Token.all_keywords;
	nk

type t = {
	table: table;
	name_to_keyword: (Symbol.t, Token.t) Hashtbl.t;
}

let get(t: t)(s: string): Symbol.t =
	t_get t.table s

let keyword(t: t)(s: Symbol.t): Token.t option =
	(* TODO: try_get helper *)
	try
		Some (Hashtbl.find t.name_to_keyword s)
	with
		Not_found -> None

let create(): t =
	let table = create_table() in
	{ table = table; name_to_keyword = create_name_to_keyword table }
