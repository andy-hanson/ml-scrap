let mock = FileIO.mock()

let test_noze = Noze.create (mock :> FileIO.t)

let compile_str(path: FileIO.path)(content: string): N.modul =
	mock#add_file path content;
	Noze.compile test_noze path


type test_result =
	| Success of N.v
	| Failure of CompileError.t

type test = {
	name: string;
	src: string;
	expect: test_result
}

let run_test({name; src; expect}: test): unit =
	let mdl = compile_str name src in
	let fn = TestU.fn_named mdl "main" in
	let actual = Interpreter.call_fn fn [| |] in
	match expect with
	| Success v ->
		Assert.equal actual v ValU.output
	| Failure _ ->
		raise U.TODO;;

run_test {
	name = "One plus one";
	src = "fn main Int
	1 + 1";
	expect = Success (N.v_int 2)
};;

run_test {
	name = "Locals";
	src = "fn main Int
	one = 1
	other-one = 1
	one + other-one";
	expect = Success (N.v_int 2)
};;

run_test {
	name = "Higher order functions";
	src = "fn main Int
	two = do-twice incr 0
	other-two = do-twice (+ .. 1) 0
	two + other-two

ft F
	Int input Int

fn do-twice Int func F target Int
	func: func target

fn incr Int x Int
	x + 1";
	expect = Success (N.v_int 4)
};;

(*TODO: test unions, ct, World, interpolation*)
