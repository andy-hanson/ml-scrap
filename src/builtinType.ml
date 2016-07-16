(*TODO: BuiltinTy*)

open N
open BuiltinTypeU

let action = ft0 "Action" t_void

let all = ArrayU.build begin fun build ->
	let build_all(tys: ty array): unit =
		ArrayU.iter tys build in

	build_all [| t_bool; t_float; t_int; t_string; t_void |];
	build_all World.tys;
	build action;
end
