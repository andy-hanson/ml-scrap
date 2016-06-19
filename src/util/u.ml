exception TODO

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
