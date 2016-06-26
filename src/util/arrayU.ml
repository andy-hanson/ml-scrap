let empty(a: 'a array): bool =
	Array.length a = 0

let head(a: 'a array): 'a =
	Array.get a 0
let tail(a: 'a array): 'a array =
	Array.sub a 1 (Array.length a - 1)

let same_length(a: 'a array)(b: 'b array): bool =
	Array.length a = Array.length b

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

let mapi(a: 'a array)(f: int -> 'a -> 'b): 'b array =
	Array.mapi f a

let map_zip(a: 'a array)(b: 'b array)(f: 'a -> 'b -> 'c): 'c array =
	assert (Array.length a = Array.length b);
	a |> Array.mapi begin fun i a_em ->
		f a_em @@ Array.get b i
	end
let zip(a: 'a array)(b: 'b array): 'c array =
	map_zip a b @@ fun x y -> x, y

let fold_map(start: 'b)(a: 'a array)(f: 'b -> 'a -> 'b * 'c): 'b * 'c array =
	let acc = ref start in
	let result_array = map a begin fun em ->
		let next_acc, result_em = f !acc em in
		acc := next_acc;
		result_em
	end in
	!acc, result_array

let filter(a: 'a array)(f: 'a -> bool): 'a array =
	BatArray.filter f a

let filter_map(a: 'a array)(f: 'a -> 'b option): 'b array =
	BatArray.filter_map f a

let find(a: 'a array)(pred: 'a -> bool): 'a option =
	try Some (BatArray.find pred a) with Not_found -> None

let find_map(a: 'a array)(f: 'a -> 'b option): 'b =
	let len = Array.length a in
	let rec recur idx =
		if idx = len then
			raise Not_found
		else
			match f (Array.get a idx) with
			| Some b ->
				b
			| None ->
				recur (idx + 1) in
	recur 0

let exists(a: 'a array)(f: 'a -> bool): bool =
	Array.exists f a

let for_all_zip(a: 'a array)(b: 'b array)(pred: 'a -> 'b -> bool): bool =
	assert (Array.length a = Array.length b);
	let rec recur(i: int) =
		i = Array.length a || (pred (Array.get a i) (Array.get b i)) && recur (i + 1) in
	recur 0


let fold(start: 'b)(a: 'a array)(f: 'b -> 'a -> 'b): 'b =
	Array.fold_left f start a

let single_of(arr: 'a array): 'a =
	assert (Array.length arr = 1);
	arr.(0)

let pair_of(arr: 'a array): 'a * 'a =
	assert (Array.length arr = 2);
	arr.(0), arr.(1)

let triple_of(arr: 'a array): 'a * 'a * 'a =
	assert (Array.length arr = 3);
	arr.(0), arr.(1), arr.(2)

let build(f: ('a -> unit) -> unit): 'a array =
	let arr = MutArray.create() in
	f (MutArray.add arr);
	MutArray.to_array arr

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

(*TODO: We only want to remove one! So don't filter!*)
let try_remove(a: 'a array)(element: 'a): 'a array option =
	let res = filter a ((!=) element) in
	if (Array.length res = Array.length a) then None else Some res

let try_remove_where(a: 'a array)(pred: 'a -> bool): ('a * 'a array) option =
	let out: 'a array = Array.make (Array.length a - 1) a.(0) in
	let rec recur(i: int) =
		if i = Array.length a then
			None
		else if pred a.(i) then begin
			for i2 = i to Array.length a - 2 do
				out.(i2) <- a.(i2 + 1)
			done;
			Some(a.(i), out)
		end else begin
			out.(i) <- a.(i);
			recur (i + 1)
		end in
	recur 0;
