type t = { name: Sym.t; value: Val.t }

let const(name: string)(value: Val.t) =
	{name = Sym.of_string name; value}

(*TODO: parameter names*)
let fn(name: string)(return_type: Type.t)(parameters: (string * Type.t) array)(exec: (unit -> Val.t) -> Val.t): t =
	let name = Sym.of_string name in
	{
		name;
		value = Val.BuiltinFn {
			Val.typ = TypeU.ft name return_type (ArrayU.map parameters (fun (name, typ) -> Sym.of_string name, typ));
			Val.exec;
		}
	}

let pop_int pop = ValU.int_of (pop())

let cond = fn "cond"
	Type.Void [| "condition", Type.Bool; "if-true", Type.Void; "if-false", Type.Void |]
	(fun _ -> raise U.TODO)

let not = fn "not"
	Type.Bool [| "b", Type.Bool |]
	(fun pop -> Val.Bool (ValU.bool_of (pop())))

let equal = fn "=="
	Type.Bool [| "a", Type.Any; "b", Type.Any |]
	(fun pop -> Val.Bool (ValU.equal (pop()) (pop())))

let cmp(name: string)(compare: int -> int -> bool) =
	fn name
		Type.Bool [| "left", Type.Int; "right", Type.Int |]
		(fun pop -> Val.Bool(compare (pop_int pop) (pop_int pop)))

let less = cmp "<" (<)

let arith(name: string)(eval: int -> int -> int) =
	fn name
		Type.Int [| "left", Type.Int; "right", Type.Int |]
		(fun pop -> Val.Int(eval (pop_int pop) (pop_int pop)))

let plus = arith "+" (+)
let minus = arith "-" (-)
let times = arith "*" ( * )
let div = arith "/" (/)

let int2float = fn "int->float"
	Type.Float [| "i", Type.Int |]
	(fun pop -> Val.Float(float_of_int (pop_int pop)))

let print = fn "print"
	Type.Void [| "printed", Type.Any |]
	begin fun pop ->
		OutputU.printf "%a\n" ValU.output (pop());
		Val.Void
	end

let tru = const "true" (Val.Bool true)
let fls = const "false" (Val.Bool false)
let pi = const "pi" (Val.Float 3.14)

let all = [|
	cond; not; equal; less; plus; minus; times; div; int2float; print;
	tru; fls; pi
|]
