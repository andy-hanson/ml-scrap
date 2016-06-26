open N

type t = { name: Sym.t; value: v }

let const(name: string)(value: v) =
	{name = Sym.of_string name; value}

let fn(name: string)(return_type: ty)(parameters: (string * ty) array)(exec: (unit -> v) -> v): t =
	let name = Sym.of_string name in
	{
		name;
		value = BuiltinFn {
			builtin_ft = TypeU.ft name return_type @@ ArrayU.map parameters (fun (name, typ) -> Sym.of_string name, typ);
			exec;
		}
	}

let pop_int pop = ValU.int_of @@ pop()

let cond = fn "cond"
	TVoid [| "condition", TBool; "if-true", TVoid; "if-false", TVoid |]
	@@ fun _ -> raise U.TODO

let not = fn "not"
	TBool [| "b", TBool |]
	@@ fun pop -> Bool(ValU.bool_of @@ pop())

let equal = fn "=="
	TBool [| "a", Any; "b", Any |]
	@@ fun pop -> Bool(ValU.equal (pop()) (pop()))

let cmp(name: string)(compare: int -> int -> bool) =
	fn name
		TBool [| "left", TInt; "right", TInt |]
		@@ fun pop -> Bool(compare (pop_int pop) (pop_int pop))

let less = cmp "<" (<)

let arith(name: string)(eval: int -> int -> int) =
	fn name
		TInt [| "left", TInt; "right", TInt |]
		@@ fun pop -> Int(eval (pop_int pop) (pop_int pop))

let plus = arith "+" (+)
let minus = arith "-" (-)
let times = arith "*" ( * )
let div = arith "/" (/)

let int2float = fn "int->float"
	TFloat [| "i", TInt |]
	@@ fun pop -> Float(float_of_int @@ pop_int pop)

let tru = const "true" @@ Bool true
let fls = const "false" @@ Bool false
let pi = const "pi" @@ Float 3.14

let all = [|
	cond; not; equal; less; plus; minus; times; div; int2float;
	tru; fls; pi
|]
