open N

let check(ty: ty)(v: v): unit =
	TypeCheckU.assert_exact (Loc.single_character 0) ty @@ ValU.ty_of v

(*TODO: move to tyU*)
let ty_fn_parameters(fn: ty_fn): parameter array =
	match fn with
	| Ft {parameters; _} -> parameters
	| Ct ct -> [| (Sym.of_string "ct-input"), Un(TyU.ct_input ct) |]

(*TODO: move to tyU*)
let ty_fn_return(fn: ty_fn): ty =
	match fn with
	| Ft {return; _} -> return
	| Ct ct -> Un(TyU.ct_output ct)

let check_fn_arguments(fn: ty_fn)(arguments: v array): unit =
	let parameters = ty_fn_parameters fn in
	if not @@ ArrayU.same_length parameters arguments then
		failwith @@ OutputU.out_to_string "Expected %i parameters, got only %i" (Array.length parameters) (Array.length arguments);
	ArrayU.iter_zip parameters arguments @@ fun (_, param_ty) -> check param_ty

let check_fn_return(fn: ty_fn)(returned: v): unit =
	check (ty_fn_return fn) returned
