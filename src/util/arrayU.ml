let iter(a: 'a array)(f: 'a -> unit): unit =
	Array.iter f a

let iteri(a: 'a array)(f: int -> 'a -> unit): unit =
	Array.iteri f a

let iter_zip(a: 'a array)(b: 'b array)(f: 'a -> 'b -> unit): unit =
	assert (Array.length a = Array.length b);
	for i = 0 to (Array.length a) - 1 do
		f (Array.get a i) (Array.get b i)
	done

let map(a: 'a array)(f: 'a -> 'b): 'b array =
	Array.map f a

let fold(start: 'b)(a: 'a array)(f: 'b -> 'a -> 'b): 'b =
	Array.fold_left f start a

let triple_of_array(arr: 'a array): 'a * 'a * 'a =
	assert ((Array.length arr) = 3);
	(Array.get arr 0, Array.get arr 1, Array.get arr 2)

(*TODO: ArrayU*)
(*TODO:RENAME*)
(*TODO: This should be the only use of BatDynArray.create*)
let build_array_0(f: ('a -> unit) -> unit): 'a array =
	let arr = BatDynArray.create() in
	f (BatDynArray.add arr);
	BatDynArray.to_array arr

let build_array(f: unit -> 'a option): 'a array =
	build_array_0 begin fun build ->
		let rec recur() =
			match f() with
			| Some x ->
				build x;
				recur()
			| None -> () in
		recur();
	end

(*TODO:RENAME*)
let build_array_2(f: unit -> 'a * bool): 'a array =
	build_array_0 begin fun build ->
		let rec recur() =
			let x, continue = f() in
			build x;
			if continue then recur() in
		recur();
	end
