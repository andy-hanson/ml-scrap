type t =
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

	| FloatToInt

	(* constants *)
	| True
	| False

let all = [| Cond; Not; Less; Add; Subtract; Times; FloatToInt; True; False |]
