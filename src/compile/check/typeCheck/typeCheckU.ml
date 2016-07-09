open N

(* This is just for `case` parameters *)
let eq(a: ty)(b: ty): bool =
	a = b

let rec assert_eq(loc: Loc.t)(expected: ty)(actual: ty): unit =
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	match expected with
	| Any | TPrimitive _ | Rt _ ->
		(* These are all nominal types. *)
		if not (eq expected actual) then fail()
	| Un _ ->
		(*TODO: Allow parameter swapping. Actually, this part may never be called, because we always do union subtyping...*)
		raise U.TODO
	| TFn f ->
		begin match f with
		| Ft {N.fname = _; N.return_type = expected_return; N.parameters = expected_params} ->
			begin match actual with
			| TFn(Ft {N.fname = _; N.return_type = actual_return; N.parameters = actual_params}) ->
				(*TODO: suggest upcasting on failure*)
				assert_eq loc expected_return actual_return;
				if (Array.length expected_params != Array.length actual_params) then fail(); (*TODO: error message*)
				ArrayU.iter_zip expected_params actual_params begin fun (_, expected) (_, actual) ->
					assert_eq loc expected actual
				end
			| _ ->
				fail()
			end
		| Ct {cname = _; ct_cases = expected_cases} ->
			begin match actual with
			| TFn(Ct {cname = _; ct_cases = actual_cases}) ->
				if (Array.length expected_cases != Array.length actual_cases) then fail();
				(*TODO: this code looks familiar. Maybe a find_case helper would be nice?*)
				ArrayU.iter expected_cases begin fun (expected_return, expected_input) ->
					let found = ArrayU.find_map actual_cases begin fun (actual_return, actual_input) ->
						OpU.op_if (eq expected_input actual_input) (fun () -> actual_return)
					end in
					let actual_return = OpU.or_else found fail in
					assert_eq loc expected_return actual_return
				end
			| _ ->
				fail()
			end
		end

(*TODO: Pretty sure this should be the same thing as assert_upcast*)
let rec assert_value_assignable(loc: Loc.t)(expected: ty)(actual: ty): unit =
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	match expected with
	| Any ->
		()
	| TPrimitive _ | Rt _ ->
		assert_eq loc actual expected
	| Un {utypes = expected_types; _} ->
		begin match actual with
		| Un {utypes = _actual_types; _} ->
			raise U.TODO
		| TPrimitive _ | Rt _ ->
			(* A union type is guaranteed to only contain simple types. *)
			if not @@ ArrayU.exists expected_types ((=) actual) then fail()
		| Any | TFn _ ->
			fail()
		end
	| TFn _ ->
		assert_upcast loc expected actual

and assert_upcast(loc: Loc.t)(expected: ty)(actual: ty): unit =
	let fail() = ErrU.raise loc @@ Err.NotExpectedType(expected, actual) in
	let foo =
		match expected with
		| TFn f -> f
		| _ -> raise U.TODO (*TODO: error?*) in
	match foo with
	| Ft {fname = _; return_type = expected_return_type; parameters = expected_parameters} ->
		begin match actual with
		| Any | TPrimitive _ | Rt _ | Un _  ->
			fail()
		| TFn f ->
			begin match f with
			| Ft {fname = _; return_type = actual_return_type; parameters = actual_parameters} ->
				assert_value_assignable loc expected_return_type actual_return_type;
				(* Functions are contravariant in parameter types *)
				ArrayU.iter_zip expected_parameters actual_parameters begin fun (_, expected_param_type) (_, actual_param_type) ->
					(*TODO: better error message for this case*)
					assert_value_assignable loc actual_param_type expected_param_type
				end
			| Ct _ ->
				raise U.TODO
			end
		end
	| Ct {cname = _; ct_cases = expected_cases} ->
		begin match actual with
		| Any | TPrimitive _ | Rt _ | Un _ ->
			fail()
		| TFn f ->
			begin match f with
			| Ft _ ->
				raise U.TODO
			| Ct {cname = _; ct_cases = actual_cases} ->
				(*
				For each expected case:
					The corresponding case is the one with the same input.
					The actual return type must subtype the expected return type.
				*)
				ArrayU.iter expected_cases begin fun (expected_return, expected_input) ->
					let corresponding_case = ArrayU.find actual_cases @@ fun (_, actual_input) -> expected_input = actual_input in
					let actual_return, _ = OpU.or_else corresponding_case @@ fun () -> raise U.TODO in (*TODO: appropriate compile error*)
					assert_value_assignable loc expected_return actual_return
				end
			end
		end

let assert_parameter_assignable(loc: Loc.t)(expected: ty)(actual: ty): unit =
	match expected with
	| Any | TPrimitive _ | Rt _ | Un _ ->
		assert_value_assignable loc expected actual
	| TFn _ ->
		(* Don't allow implicit upcasting of function parameters *)
		assert_eq loc expected actual

let join(loc: Loc.t)(types: ty array): ty =
	(*TODO actual join algorithm*)
	let t = Array.get types 0 in
	ArrayU.iter types begin fun typ ->
		ErrU.check (eq t typ) loc @@ Err.CombineTypes(t, typ)
	end;
	t
