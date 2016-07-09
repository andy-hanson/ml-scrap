open N
(*open BuiltinTyU*)

let action = TFn(Ft {
	fname = Sym.of_string "Action";
	return_type = t_void;
	parameters = [||]
})

let foo = [|
	t_bool; t_float; t_int; t_string; t_void;
	action
|]

let all = Array.append foo World.types
