Printexc.record_backtrace true

exception TODO
let todo(): 'a =
	(* This wierd code makes the stack trace reach down to here. *)
	let _ = raise TODO in
	raise TODO

let compose(f: 'a -> 'b)(g: 'b -> 'c)(a: 'a) =
	g (f a)

let fail(reason: string): 'a =
	failwith reason

let do_times(times: int)(action: unit -> unit): unit =
	for _ = 1 to times do
		action()
	done

let returning(value: 'a)(action: 'a -> unit): 'a =
	action value;
	value

let loop0(f: (unit -> 'res) -> 'res): 'res =
	let rec loop() =
		f loop in
	loop()

let loop(state: 'state)(f: ('state -> 'res) -> 'state -> 'res): 'res =
	let rec loop(state: 'state): 'res =
		f loop state in
	loop state

let loop2(a: 'a)(b: 'b)(f: ('a -> 'b -> 'res) -> 'a -> 'b -> 'res): 'res =
	let rec loop(a: 'a)(b: 'b): 'res =
		f loop a b in
	loop a b

let loop3(a: 'a)(b: 'b)(c: 'c)(f: ('a -> 'b -> 'c -> 'res) -> 'a -> 'b -> 'c -> 'res): 'res =
	let rec loop(a: 'a)(b: 'b)(c: 'c): 'res =
		f loop a b c in
	loop a b c
