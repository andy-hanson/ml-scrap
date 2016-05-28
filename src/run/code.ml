type bytecode =
	| Call of func
	| CallBuiltin of Builtins.builtin
	| Const of Val.t
	| Construct of Type.record
	| Drop
	(* Load a value from `int` entries earlier in the stack *)
	| Load of int
	(* Goto-like codes store index of bytecode to move to. *)
	| Goto of int
	| GotoIfFalse of int
	| Return
	(* For `a = b; c`, we push `b`, then eval `c` (which may fetch `a`); then remove `a` from under it. *)
	| UnLet
and t = bytecode array
and parameter = {
	name: Symbol.t
	(*TODO*)
	(* type: Type.t *)
}
and func = {
	(* ast: Ast.decl_val; *)
	(*TODO:RENAME*)
	fname: Symbol.t;
	params: parameter array;
	mutable code: t
}

(*TODO: hide somehow *)
let empty_func_from_ast(Ast.DeclVal(_, name, Ast.Fn(Ast.Signature(_, _, params), expr))): func =
	{
		fname = name;
		params =
			(let to_param(Ast.LocalDeclare(_, name, typ)): parameter =
				{ name = name } in
			Array.map to_param params);
		(* Code is empty for now, will be written to in writeCode.ml *)
		code = [| |]
	}

let func_arity({params}: func): int =
	Array.length params


(* boilerplate *)

let output_parameter(out: 'a BatIO.output)({name}: parameter): unit =
	OutputU.out out "{ name: %a }" Symbol.output name
let rec output_func(out: 'a BatIO.output)({fname; params; code}: func): unit =
	OutputU.out out "{ fname: %a, params: %a, code: %a }" Symbol.output fname (OutputU.out_array output_parameter) params output code
and output_code(out: 'a BatIO.output)(c: bytecode): unit =
	let str = BatIO.nwrite out in
	match c with
	| Call f ->
		OutputU.out out "CallFn(%a)" Symbol.output f.fname
	| CallBuiltin b ->
		OutputU.out out "CallBuiltin(%a)" Builtins.output b
	| Const v ->
		OutputU.out out "Const(%a)" Val.output v
	| Construct r ->
		OutputU.out out "Construct(%a)" Symbol.output r.Type.rname
	| Drop ->
		str "Drop"
	| Load i ->
		OutputU.out out "Load(%d)" i
	| Goto i ->
		OutputU.out out "Goto(%d)" i
	| GotoIfFalse i ->
		OutputU.out out "GotoIfFalse(%d)" i
	| Return ->
		str "Return"
	| UnLet ->
		str "UnLet"
and output(out: 'a BatIO.output)(code: t): unit =
	OutputU.out out "Code(%a)" (OutputU.out_array output_code) code
