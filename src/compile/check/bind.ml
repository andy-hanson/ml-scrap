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

let value_binding {values; _} access = Bindings.get values access
let type_binding {types; _} access = TypeBindings.get types access

module SymMapU = MapU.Make(Symbol.SymMap)

type scope = Binding.t Symbol.SymMap.t

let builtins_scope(ctx: CompileContext.t): scope =
	let m1 = SymMapU.make Builtins.all begin fun b ->
		let name = CompileContext.symbol ctx (Builtins.name b) in
		(name, Binding.Builtin b)
	end in
	let m2 = SymMapU.make Type.builtins begin fun b ->
		let name = CompileContext.symbol ctx (Type.builtin_name b) in
		(name, Binding.BuiltinType b)
	end in
	SymMapU.union m1 m2

let augment_scope(base: scope)(Ast.LocalDeclare(_, name) as local): scope =
	Symbol.SymMap.add name (Binding.Local local) base

(*TODO:neater*)
let augment_scope_param(base: scope)(Ast.Parameter(_, name, _) as parameter): scope =
	Symbol.SymMap.add name (Binding.Parameter parameter) base
let augment_scope_many(base: scope)(more: Ast.parameter array): scope =
	ArrayU.fold base more augment_scope_param

let each_decl(Ast.Modul(_, decls))(fn: Ast.decl -> unit) =
	ArrayU.iter decls fn

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
	(*TODO: inline?*)
	let bind_param_type(scope: scope)(Ast.Parameter(_, _, (Ast.TypeAccess(loc, name) as typ))): unit =
		write_type_binding scope typ loc name in

	let rec add_expr_bindings(scope: scope)(Ast.Expr(loc, kind) as expr): unit =
		match kind with
		| Ast.Access name ->
			write_binding scope expr loc name
		| Ast.Call(called, arguments) ->
			add_expr_bindings scope called;
			ArrayU.iter arguments (add_expr_bindings scope)
		| Ast.Let(declare, value, expr) ->
			add_expr_bindings scope value;
			let scope = augment_scope scope declare in
			add_expr_bindings scope expr
		| Ast.Literal(_) ->
			()
		| Ast.Seq(a, b) ->
			add_expr_bindings scope a;
			add_expr_bindings scope b in

	let add_fn_bindings(Ast.Fn(Ast.Signature(_, (Ast.TypeAccess(return_loc, return_name) as return_type), params), body)): unit =
		write_type_binding base_scope return_type return_loc return_name;
		ArrayU.iter params (bind_param_type base_scope);
		add_expr_bindings (augment_scope_many base_scope params) body in

	let add_rec_bindings(Ast.Rec properties): unit =
		ArrayU.iter properties begin fun (Ast.Property(_, _, (Ast.TypeAccess(loc, name) as typ))) ->
			write_type_binding base_scope typ loc name
		end in

	each_decl modul begin function
		| Ast.Val (Ast.DeclVal(_, _, kind)) ->
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

(* let output(out: 'a OutputU.t)(bindings: bindings): unit =
	OutputU.out_hashtbl Ast.output_expr Binding.output out bindings
 *)
