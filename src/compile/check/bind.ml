(* TODO: lots of cleanup in this file *)

(* Maps Access to type *)
module Bindings = Lookup.T(struct type t = Ast.expr end)
type value_bindings =  Binding.t Bindings.t
module TypeBindings = Lookup.T(struct type t = Ast.typ end)
type type_bindings = Binding.t TypeBindings.t

type t = {
	values: value_bindings;
	types: type_bindings
}

let value_binding {values} access = Bindings.get values access
let type_binding {types} access = TypeBindings.get types access

module SymMapU = U.MapU(Symbol.SymMap)

type scope = Binding.t Symbol.SymMap.t

let builtins_scope(ctx: CompileContext.t): scope =
	let m1 = SymMapU.make Builtins.all (fun b ->
		let name = CompileContext.symbol ctx (Builtins.name b) in
		(name, Binding.Builtin b)) in
	let m2 = SymMapU.make Type.builtins (fun b ->
		let name = CompileContext.symbol ctx (Type.builtin_name b) in
		(name, Binding.BuiltinType b)) in
	SymMapU.union m1 m2


let augment_scope(base: scope)(Ast.LocalDeclare(_, name, _) as local): scope =
	Symbol.SymMap.add name (Binding.Local local) base

let augment_scope_many(base: scope)(more: Ast.local_declare array): scope =
	Array.fold_left augment_scope base more

let each_decl(Ast.Modul(_, decls))(fn: Ast.decl -> unit) =
	Array.iter fn decls

let get_base_scope(ctx: CompileContext.t)(modul: Ast.modul): scope =
	let map: scope ref = ref (builtins_scope ctx) in
	let add loc name binding =
		CompileError.check (not (Symbol.SymMap.mem name !map)) loc (CompileError.NameAlreadyBound name);
		map := Symbol.SymMap.add name binding !map in
	(*TODO:FOLD and don't mutate map variable*)
	each_decl modul begin function
		| Ast.Val (Ast.DeclVal(loc, sym, _) as v) ->
			add loc sym (Binding.Declared v)
		| Ast.Type (Ast.DeclType(loc, sym, _) as t) ->
			add loc sym (Binding.DeclaredType t)
	end;
	!map

let bind(ctx: CompileContext.t)(modul: Ast.modul): t =
	let base_scope = get_base_scope ctx modul in
	let bindings: value_bindings = Bindings.create() in
	let type_bindings: type_bindings = TypeBindings.create() in

	(* access should be an Ast.Access with loc loc and name name *)
	let write_binding(scope: scope)(access: Ast.expr)(loc: Loc.t)(name: Symbol.t): unit =
		let binding = try
			Symbol.SymMap.find name scope
		with
			Not_found ->
				CompileError.raise loc (CompileError.CantBind name) in
		Bindings.set bindings access binding in

	(*TODO: just have access as paremeter, not loc+name*)
	let write_type_binding(scope: scope)(access: Ast.typ)(loc: Loc.t)(name: Symbol.t): unit =
		(*TODO:REUSE CODE*)
		let binding = try
			Symbol.SymMap.find name scope
		with
			Not_found ->
				CompileError.raise loc (CompileError.CantBind name) in
		TypeBindings.set type_bindings access binding in

	(*TODO: naming*)
	let bind_declare_type(scope: scope)(Ast.LocalDeclare(_, _, typ)) =
		let Ast.TypeAccess(loc, name) = typ in
		write_type_binding scope typ loc name in

	let rec add_expr_bindings(scope: scope)(Ast.Expr(loc, kind) as expr): unit =
		match kind with
		| Ast.Access name ->
			write_binding scope expr loc name
		| Ast.Call(called, arguments) ->
			add_expr_bindings scope called;
			Array.iter (add_expr_bindings scope) arguments
		| Ast.Let(declare, value, expr) ->
			bind_declare_type scope declare;
			add_expr_bindings scope value;
			let scope = augment_scope scope declare in
			add_expr_bindings scope expr
		| Ast.Literal(_) ->
			()
		| Ast.Seq(a, b) ->
			add_expr_bindings scope a;
			add_expr_bindings scope b in

	let add_fn_bindings(Ast.Fn(Ast.Signature(loc, (Ast.TypeAccess(return_loc, return_name) as return_type), params), body)): unit =
		write_type_binding base_scope return_type return_loc return_name;
		Array.iter (bind_declare_type base_scope) params;
		add_expr_bindings (augment_scope_many base_scope params) body in

	let add_rec_bindings(Ast.Rec properties): unit =
		let foo(Ast.Property(_, _, (Ast.TypeAccess(loc, name) as typ))): unit =
			write_type_binding base_scope typ loc name in
		Array.iter foo properties in

	each_decl modul begin function
		| Ast.Val (Ast.DeclVal(loc, sym, kind)) ->
			begin match kind with
			| Ast.Fn(_, _) as f ->
				add_fn_bindings f
			end
		| Ast.Type(Ast.DeclType(_, _, r)) ->
			add_rec_bindings r
	end;

	(*TODO: rename variables to match record field names*)
	{ values = bindings; types = type_bindings }


(* boilerplate *)

(* let output(out: 'a BatIO.output)(bindings: bindings): unit =
	OutputU.out_hashtbl Ast.output_expr Binding.output out bindings
 *)
