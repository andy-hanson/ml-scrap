type builtin =
	(* Special *)
	| Cond

	(* Logic *)
	| Not

	(* Comparison *)
	| Less

	(* Arithmetic *)
	| Add
	| Subtract
	| Times

	(* constants *)
	| True
	| False

let all = [| Cond; Not; Less; Add; Subtract; Times; True; False |]

let name = function
	| Cond -> "cond"
	| Not -> "not"
	| Less -> "<"
	| Add -> "+"
	| Subtract -> "-"
	| Times -> "*"
	| True -> "true"
	| False -> "false"

let type_of(b: builtin): Type.t =
	match b with
	| Cond ->
		(*TODO: parameterized types*)
		raise U.TODO
	| Not ->
		Type.Fn(Type.Builtin Type.Bool, [| Type.Builtin Type.Bool |])
	| Less ->
		Type.Fn(Type.Builtin Type.Bool, [| Type.Builtin Type.Int; Type.Builtin Type.Int |])
	| Add | Subtract | Times ->
		(*TODO: interface type, so it can work on floats too*)
		Type.Fn(Type.Builtin Type.Int, [| Type.Builtin Type.Int; Type.Builtin Type.Int |])
	| True ->
		Type.Builtin Type.Bool
	| False ->
		Type.Builtin Type.Bool

let value = function
	| True -> Val.Bool true
	| False -> Val.Bool false
	| _ -> failwith "not a value (todo: lambdas)"

(* TODO: replace this with named+typed parameters list *)
let arity(b: builtin): int =
	match b with
	| Cond -> 3
	| Not -> 1
	| Less -> 2
	| Add -> 2
	| Subtract -> 2
	| Times -> 2
	| b -> failwith ("not a function: " ^ (name b))

let output(out: 'a BatIO.output)(b: builtin): unit =
	BatIO.nwrite out (name b)
