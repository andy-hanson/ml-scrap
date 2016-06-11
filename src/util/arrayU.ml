let iter(a: 'a array)(f: 'a -> unit): unit =
	Array.iter f a

let iteri(a: 'a array)(f: int -> 'a -> unit): unit =
	Array.iteri f a

let iter_zip(a: 'a array)(b: 'b array)(f: 'a -> 'b -> unit): unit =
	assert (Array.length a = Array.length b);
	for i = 0 to (Array.length a) - 1 do
		f a.(i) b.(i)
	done

let map(a: 'a array)(f: 'a -> 'b): 'b array =
	Array.map f a

let fold(start: 'b)(a: 'a array)(f: 'b -> 'a -> 'b): 'b =
	Array.fold_left f start a

let triple_of_array(arr: 'a array): 'a * 'a * 'a =
	assert ((Array.length arr) = 3);
	arr.(0), arr.(1), arr.(2)

let build(f: ('a -> unit) -> unit): 'a array =
	let arr = BatDynArray.create() in
	f (BatDynArray.add arr);
	BatDynArray.to_array arr

let build_loop(f: unit -> 'a * bool): 'a array =
	build begin fun build ->
		let rec recur() =
			let x, continue = f() in
			build x;
			if continue then recur() in
		recur();
	end

let build_until_none(f: unit -> 'a option): 'a array =
	build begin fun build ->
		let rec recur() =
			match f() with
			| Some x ->
				build x;
				recur()
			| None -> () in
		recur();
	end
