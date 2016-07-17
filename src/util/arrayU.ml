let empty(a: 'a array): bool =
	Array.length a = 0

let same_length(a: 'a array)(b: 'b array): bool =
	Array.length a = Array.length b

let iter(a: 'a array)(f: 'a -> unit): unit =
	Array.iter f a

let iteri(a: 'a array)(f: int -> 'a -> unit): unit =
	Array.iteri f a

let iter_zip(a: 'a array)(b: 'b array)(f: 'a -> 'b -> unit): unit =
	assert (same_length a b);
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

let find_zip(a: 'a array)(b: 'b array)(find: 'a -> 'b -> 'c option): 'c option =
	assert (same_length a b);
	let rec recur(i: int): 'c option =
		if i > Array.length a then
			None
		else
			match find a.(i) b.(i) with
			| None -> recur @@ i + 1
			| some -> some in
	recur 0

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

let find_index(a: 'a array)(pred: 'a -> bool): int option =
	try
		Some(BatArray.findi pred a)
	with Not_found ->
		None

let find_map(a: 'a array)(f: 'a -> 'b option): 'b option =
	let rec recur(idx: int): 'b option =
		if idx = Array.length a then
			None
		else
			OpU.or_try (f a.(idx)) @@ fun () -> recur @@ idx + 1 in
	recur 0

let exists(a: 'a array)(f: 'a -> bool): bool =
	Array.exists f a


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

let build_and_return(f: ('a -> unit) -> 'b): 'a array * 'b =
	let arr = BatDynArray.create() in
	let res = f @@ BatDynArray.add arr in
	BatDynArray.to_array arr, res

type ('a, 'b) builder =
	| Cont of 'a
	| Done of 'b
let build_with_first(a: 'a)(f: unit -> ('a, 'b) builder): 'a array * 'b =
	build_and_return begin fun build ->
		build a;
		let rec recur(): 'b =
			match f() with
			| Cont a ->
				build a;
				recur()
			| Done b ->
				b in
		recur()
	end

let build(f: ('a -> unit) -> unit): 'a array =
	fst @@ build_and_return f

let build_loop(f: unit -> 'a * bool): 'a array =
	build begin fun build ->
		let rec recur() =
			let x, continue = f() in
			build x;
			if continue then recur() in
		recur();
	end

let rec build_until_none_worker(f: unit -> 'a option)(build: 'a -> unit): unit =
	match f() with
	| Some x ->
		build x;
		build_until_none_worker f build
	| None ->
		()

let build_until_none(f: unit -> 'a option): 'a array =
	build @@ build_until_none_worker f

let build_until_none_with_first(first: 'a)(f: unit -> 'a option): 'a array =
	build begin fun build ->
		build first;
		build_until_none_worker f build
	end

let build_fold(start: 'f)(fold: 'f -> 'a option * 'f option): 'a array =
	build begin fun build ->
		let rec recur(x) =
			let here, next = fold x in
			OpU.may here build;
			OpU.may next recur in
		recur start
	end

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
	recur 0

let try_remove(a: 'a array)(element: 'a): 'a array option =
	OpU.map (try_remove_where a @@ (=) element) @@ fun (_, remaining) -> remaining

let partial(a: 'a array)(b: 'b array)(iter: 'a -> 'b -> unit): 'a array =
	let n_remaining = Array.length a - Array.length b in
	assert (n_remaining >= 0);
	for i = 0 to Array.length b - 1 do
		iter a.(n_remaining + i) b.(i)
	done;
	Array.sub a 0 n_remaining

let rtail(a: 'a array): 'a array =
	Array.sub a 0 (Array.length a - 1)
let last(a: 'a array): 'a =
	Array.get a (Array.length a - 1)


let output_elements ?(delimeter=", ")(output_element: ('a, 'o) OutputU.printer)(out: 'o OutputU.t)(arr: 'a array): unit =
	iteri arr begin fun idx em ->
		output_element out em;
		if idx != Array.length arr - 1 then
			OutputU.str out delimeter
	end

(*TODO: Use BatArray.print*)
let output(output: ('a, 'o) OutputU.printer)(out_channel: 'o OutputU.t)(arr: 'a array): unit =
	OutputU.out out_channel "[%a]" (output_elements output) arr

let eq(a: 'a array)(b: 'b array)(eq: 'a -> 'b -> bool): bool =
	same_length a b &&
		not @@ OpU.empty @@ find_zip a b @@ fun a b -> OpU.op_if (eq a b) @@ fun () -> ()
