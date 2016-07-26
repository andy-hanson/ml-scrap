open N.V
open N.Ty

let check(ty: ty)(v: v): unit =
	TypeCheckU.assert_exact (Loc.single_character 0) ty @@ ValU.ty_of v

let check_fn_arguments({parameters; _}: ft)(arguments: v array): unit =
	if not @@ ArrayU.same_length parameters arguments then
		failwith @@ OutputU.out_to_string "Expected %i parameters, got only %i" (Array.length parameters) (Array.length arguments);
	ArrayU.iter_zip parameters arguments @@ fun (_, param_ty) -> check param_ty

let check_fn_return({return; _}: ft)(returned: v): unit =
	check return returned
