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
