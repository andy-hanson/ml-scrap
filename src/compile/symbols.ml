module Table = Lookup.Make(struct
	include String
	let hash = Hashtbl.hash
end)
type table = Symbol.t Table.t

type t = { table: table; name_to_keyword: Token.t Symbol.Lookup.t; builtins_scope: Scope.t }

let get_sym(t: table)(s: string): Symbol.t =
	Table.get_or_update t s (fun () -> Symbol.make s)

let get t = get_sym t.table

let keyword(t: t) = Symbol.Lookup.try_get t.name_to_keyword

let builtins_scope t = t.builtins_scope

let create(): t =
	let table = Table.create_with_size 16384 in
	let name_to_keyword =
		Symbol.Lookup.build_from_values Token.all_keywords begin fun keyword ->
			get_sym table (Token.keyword_to_string keyword)
		end in
	let builtins_scope =
		let m1 = Symbol.Map.make Builtin.all begin fun b ->
			get_sym table (BuiltinU.name b), Binding.Builtin b
		end in
		let m2 = Symbol.Map.make Type.builtins begin fun b ->
			get_sym table (TypeU.builtin_name b), Binding.BuiltinType b
		end in
		(* There are no shared names *)
		Symbol.Map.union (fun _ _ _ -> assert false) m1 m2 in
	{ table; name_to_keyword; builtins_scope }
