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

let type_of(builtin: builtin): Type.t =
	let i = Type.Builtin Type.Int in
	let b = Type.Builtin Type.Bool in
	let f r a = Type.t_fn r a in
	match builtin with
	| Cond ->
		(*TODO: parameterized type!*)
		f i [| b; i; i |]
	| Not ->
		f b [| b; b |]
	| Less ->
		f b [| i; i |]
	| Add | Subtract | Times ->
		(*TODO: interface type, so it can work on floats too*)
		f i [| i; i |]
	| True ->
		b
	| False ->
		b

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

let output(out: 'a OutputU.t)(b: builtin): unit =
	OutputU.str out (name b)
