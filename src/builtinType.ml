open Type

let print = TypeU.t_rc
	(Sym.of_string "Print")
	[| TypeU.property (Sym.of_string "value") Type.Any |]

let all = [|
	(*TODO:Any*)
	Bool; Float; Int; Void;
	print
|]
