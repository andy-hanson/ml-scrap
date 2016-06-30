(*TODO: combine this with builtinType.ml*)
(*TODO: builtins should just get 'pop', not the full state.*)

open N

(*TODO: is this data structure really needed? We just need the lookup from name->value.*)
type t = { name: Sym.t; value: v }

let const(name: string)(value: v) =
	{name = Sym.of_string name; value}

(*TODO:RENAME*)
let ffnn(name: string)(return_type: ty)(parameters: (string * ty) array)(exec: N.interpreter_state -> unit): t =
	let name = Sym.of_string name in
	{
		name;
		value = Fn(BuiltinFn {
			builtin_ty_fn = Ft(TypeU.ft name return_type @@ ArrayU.map parameters @@ fun (name, typ) -> Sym.of_string name, typ);
			exec
		})
	}

let fn(name: string)(return_type: ty)(parameters: (string * ty) array)(exec: (unit -> v) -> v): t =
	ffnn name return_type parameters @@ fun state -> State.push state @@ exec @@ fun () -> State.pop state

(*TODO: this should have a generic type, meaning the action can return any value (not just Void).*)
let do_action = ffnn "do"
	t_void [| "fn", BuiltinType.action |]
	@@ fun _ -> raise U.TODO
(*TODO:NEATER*)
let do_value = match do_action.value with | Fn(BuiltinFn f) -> f | _ -> assert false

let pop_int pop = ValU.int_of @@ pop()

let cond = fn "cond"
	t_void [| "condition", t_bool; "if-true", t_void; "if-false", t_void |]
	@@ fun _ -> raise U.TODO
(*TODO:NEATER*)
let cond_value = match cond.value with | Fn(BuiltinFn f) -> f | _ -> assert false

let not = fn "not"
	t_bool [| "b", t_bool |]
	@@ fun pop -> v_bool(ValU.bool_of @@ pop())

let equal = fn "=="
	t_bool [| "a", Any; "b", Any |]
	@@ fun pop -> v_bool(ValU.equal (pop()) (pop()))

let cmp(name: string)(compare: int -> int -> bool) =
	fn name
		t_bool [| "left", t_int; "right", t_int |]
		@@ fun pop -> v_bool(compare (pop_int pop) (pop_int pop))

let less = cmp "<" (<)

let arith(name: string)(eval: int -> int -> int) =
	fn name
		t_int [| "left", t_int; "right", t_int |]
		@@ fun pop -> v_int(eval (pop_int pop) (pop_int pop))

let plus = arith "+" (+)
let minus = arith "-" (-)
let times = arith "*" ( * )
let div = arith "/" (/)

let int2float = fn "int->float"
	t_float [| "i", t_int |]
	@@ fun pop -> v_float(float_of_int @@ pop_int pop)

let tru = const "true" @@ v_bool true
let fls = const "false" @@ v_bool false
let pi = const "pi" @@ v_float 3.14

let all = [|
	do_action; cond; not; equal; less; plus; minus; times; div; int2float;
	tru; fls; pi
|]
