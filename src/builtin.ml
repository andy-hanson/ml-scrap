open N

(*TODO:RENAME*)
let fffnnn(name: string)(return: ty)(parameters: (string * ty) array)(exec: interpreter_state -> unit): builtin_fn =
	{
		builtin_ty_fn = Ft(TyU.ft (Sym.of_string name) return @@ ArrayU.map parameters @@ fun (name, ty) -> Sym.of_string name, ty);
		exec
	}

(*TODO:RENAME*)
let do_value = fffnnn "do"
	t_void [| "fn", BuiltinType.action |]
	@@ fun state -> State.call state @@ State.pop state

let cond_value = fffnnn "cond"
	t_int [| "condition", t_bool; "if-true", t_int; "if-false", t_int |]
	@@ fun _ -> raise U.TODO


let pop_int(pop: unit -> v): int =
	ValU.int_of @@ pop()


let all = Sym.Map.build begin fun (build_sym: Sym.t -> v -> unit) ->
	let build(s: string) = build_sym (Sym.of_string s) in

	(*TODO:RENAME*)
	let fnz(fn: builtin_fn): unit =
		build_sym (ValU.builtin_fn_name fn) (Fn(BuiltinFn fn)) in
	(*TODO:RENAME*)
	let ffnn(name: string)(return: ty)(parameters: (string * ty) array)(exec: interpreter_state -> unit): unit =
		fnz @@ fffnnn name return parameters exec in
	(*TODO:RENAME*)
	let fn(name: string)(return: ty)(parameters: (string * ty) array)(exec: (unit -> v) -> v): unit =
		ffnn name return parameters @@ fun state -> State.push state @@ exec @@ fun () -> State.pop state in

	build "true" (v_bool true);
	build "false" (v_bool false);
	build "pi" (v_float 3.14);

	fnz do_value;
	fnz cond_value;

	fn "not"
		t_bool [| "b", t_bool |]
		(fun pop -> v_bool(not @@ ValU.bool_of @@ pop()));

	fn "=="
		t_bool [| "a", Any; "b", Any |]
		(fun pop -> v_bool(ValU.equal (pop()) (pop())));

	let cmp(name: string)(compare: int -> int -> bool) =
		fn name
			t_bool [| "left", t_int; "right", t_int |]
			@@ fun pop -> v_bool(compare (pop_int pop) (pop_int pop)) in

	cmp "<" (<);
	cmp "<=" (<=);
	cmp ">" (>);
	cmp ">=" (>=);

	let arith(name: string)(eval: int -> int -> int) =
		fn name
			t_int [| "left", t_int; "right", t_int |]
			@@ fun pop -> v_int(eval (pop_int pop) (pop_int pop)) in

	arith "+" (+);
	arith "-" (-);
	arith "*" ( * );
	arith "/" (/);

	fn "int->float"
		t_float [| "i", t_int |]
		@@ fun pop -> v_float(float_of_int @@ pop_int pop)
end
