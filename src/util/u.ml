exception TODO

let do_times(times: int)(action: unit -> unit): unit =
	for _ = 1 to times do
		action()
	done

let returning(value: 'a)(action: 'a -> unit): 'a =
	action value;
	value

let op_if(cond: bool)(make_some: unit -> 'a): 'a option =
	if cond then
		Some (make_some ())
	else
		None

let mod_ref(r: 'a ref)(f: 'a -> 'a): unit =
	r := f !r

let assert_equal(output: ('a, 'o) OutputU.printer)(a: 'a)(b: 'a): unit =
	if not (a = b) then
		failwith (OutputU.out_to_string "Expected %a = %a" output a output b)
