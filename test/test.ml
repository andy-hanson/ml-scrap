let mock = FileIO.mock()

let test_noze = Noze.create (mock :> FileIO.t)

let compile_str(path: FileIO.path)(content: string): N.modul =
	mock#add_file path content;
	Noze.compile test_noze path

type test_result =
	| Success of N.v
	| Failure of Err.t

type test = {
	name: string;
	src: string
}

let run_test({name; src}: test): unit =
	let mdl = compile_str name src in
	let fn = TestU.fn_named mdl "main" in
	let actual = Interpreter.call_fn fn [| |] in
	assert (actual = N.v_void);;

run_test {
	name = "1 + 1";
	src = "
fn main Void
	ck (1 + 1) == 2"
};;

run_test {
	name = "Recursion";
	src ="
fn main Void
	ck (factorial 6) == 720

fn factorial Int x Int
	less = x < 2
	cond less 1: x * factorial: x - 1"
};;

run_test {
	name = "Let";
	src = "
fn main Void
	a = 1
	b = 2
	ck a == 1
	ck b == 2"
};;

(*TODO need to be able to read values back out to really test this!*)
run_test {
	name = "Rt";
	src ="
rt Point
	x Int
	y Int

fn main Void
	ck 0 == 0"
};;

run_test {
	name = "Un";
	src = "
fn main Void
	ck is-float? 0.0
	ck not: is-float? 0

un FloatOrInt
	Float
	Int

fn is-float? Bool x FloatOrInt
	cs x
		Float @@ f
			true
		Int @@ i
			false"
};;

run_test {
	name = "Patterns";
	src = "
fn main Void
	p = Point 1 2
	x y = p
	ck x == 1
	ck y == 2
	ck sum 1 == 1
	ck sum p == 3

rt Point
	x Int
	y Int

un IntOrPoint
	Int
	Point

fn sum Int ip IntOrPoint
	cs ip
		Int @@ i
			i
		Point x y
			x + y"
};;

run_test {
	name = "Higher order functions";
	src = "
fn main Void
	two = do-twice incr 0
	other-two = do-twice (+ .. 1) 0
	ck two == other-two

ft F
	Int input Int

fn do-twice Int func F target Int
	func: func target

fn incr Int x Int
	x + 1"
};;

(* Requires polymorphic equality
run_test {
	name = "String interpolation";
	src = "
fn main Void
	ck \"1 + 1 = {int->string: 1 + 1}!\" == \"1 + 1 = 2!\""
};;
*)

(*TODO: World tests w/ mock world*)
