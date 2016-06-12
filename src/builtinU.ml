open Builtin

let name = function
	| Cond -> "cond"
	| Not -> "not"
	| Less -> "<"

	| Add -> "+"
	| Subtract -> "-"
	| Times -> "*"
	| FloatToInt -> "float->int"

	| True -> "true"
	| False -> "false"

let type_of(builtin: t): Type.t =
	let i = Type.Int in
	let b = Type.Bool in
	let f = Type.Float in
	let fn r a = TypeU.t_fn r a in
	match builtin with
	| Cond ->
		fn i [| b; i; i |]
	| Not ->
		fn b [| b; b |]
	| Less ->
		fn b [| i; i |]
	| Add | Subtract | Times ->
		fn i [| i; i |]
	| FloatToInt ->
		fn i [| f |]
	| True ->
		b
	| False ->
		b

let value = function
	| True ->
		Val.Bool true
	| False ->
		Val.Bool false
	| _ ->
		failwith "not a value"

let arity(b: t): int =
	match b with
	| Cond -> 3
	| Not -> 1
	| Less -> 2
	| Add -> 2
	| Subtract -> 2
	| Times -> 2
	| FloatToInt -> 1
	| b -> failwith ("not a function: " ^ (name b))

let output(out: 'o OutputU.t)(b: t): unit =
	OutputU.str out (name b)
