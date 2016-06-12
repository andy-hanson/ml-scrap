exception TODO

let do_times(times: int)(action: unit -> unit): unit =
	for _ = 1 to times do
		action()
	done

let returning(value: 'a)(action: 'a -> unit): 'a =
	action value;
	value
