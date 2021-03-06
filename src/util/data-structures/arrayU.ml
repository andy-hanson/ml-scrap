let nil: 'a array =
	[||]

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
	a |> Array.mapi @@ fun i a_em ->
		f a_em @@ Array.get b i

let map_to_list(a: 'a array)(f: 'a -> 'b): 'b list =
	U.loop2 (Array.length a - 1) [] @@ fun loop idx lst ->
		if idx < 0 then
			lst
		else
			loop (idx - 1) @@ (f a.(idx)) :: lst

let zip(a: 'a array)(b: 'b array): 'c array =
	map_zip a b @@ fun x y -> x, y

let find_zip(a: 'a array)(b: 'b array)(find: 'a -> 'b -> 'c option): 'c option =
	assert (same_length a b);
	U.loop 0 @@ fun loop i ->
		if i >= Array.length a then
			None
		else
			match find a.(i) b.(i) with
			| None -> loop @@ i + 1
			| some -> some

let fold_map(start: 'b)(a: 'a array)(f: 'b -> 'a -> 'b * 'c): 'b * 'c array =
	let acc = ref start in
	let result_array = map a @@ fun em ->
		let next_acc, result_em = f !acc em in
		acc := next_acc;
		result_em in
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
	U.loop 0 @@ fun loop idx ->
		if idx = Array.length a then
			None
		else
			OpU.or_try (f a.(idx)) @@ fun () -> loop @@ idx + 1

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
let build_loop0_with_first(a: 'a)(f: unit -> ('a, 'b) builder): 'a array * 'b =
	build_and_return @@ fun build ->
		build a;
		U.loop0 @@ fun loop ->
			match f() with
			| Cont a ->
				build a;
				loop()
			| Done b ->
				b

let build_loop0(f: unit -> ('a, 'b) builder): 'a array * 'b =
	build_and_return @@ fun build ->
		U.loop0 @@ fun loop ->
			match f() with
			| Cont a ->
				build a;
				loop()
			| Done b ->
				b

let build(f: ('a -> unit) -> unit): 'a array =
	fst @@ build_and_return f

let build_loop(f: unit -> 'a * bool): 'a array =
	build @@ fun build ->
		U.loop0 @@ fun loop ->
			let x, continue = f() in
			build x;
			if continue then loop()

let build_until_none_worker(f: unit -> 'a option)(build: 'a -> unit): unit =
	U.loop0 @@ fun loop ->
		match f() with
		| Some x ->
			build x;
			loop()
		| None ->
			()

let build_until_none(f: unit -> 'a option): 'a array =
	build @@ build_until_none_worker f

let build_until_none_with_first(first: 'a)(f: unit -> 'a option): 'a array =
	build @@ fun build ->
		build first;
		build_until_none_worker f build

let build_fold(start: 'f)(fold: 'f -> 'a option * 'f option): 'a array =
	build @@ fun build ->
		U.loop start @@ fun loop x ->
			let here, next = fold x in
			OpU.may here build;
			OpU.may next loop

let try_remove_where(a: 'a array)(pred: 'a -> bool): ('a * 'a array) option =
	let out: 'a array = Array.make (Array.length a - 1) a.(0) in
	U.loop 0 @@ fun loop i ->
		if i = Array.length a then
			None
		else if pred a.(i) then begin
			for i2 = i to Array.length a - 2 do
				out.(i2) <- a.(i2 + 1)
			done;
			Some (a.(i), out)
		end else begin
			out.(i) <- a.(i);
			loop @@ i + 1
		end

let try_remove(a: 'a array)(element: 'a): 'a array option =
	OpU.map (try_remove_where a @@ (=) element) @@ fun (_, remaining) -> remaining

let partial_iter(a: 'a array)(b: 'b array)(iter: 'a -> 'b -> unit): unit =
	let n_remaining = Array.length a - Array.length b in
	assert (n_remaining >= 0);
	for i = 0 to Array.length b - 1 do
		iter a.(n_remaining + i) b.(i)
	done

let tail(a: 'a array): 'a array =
	Array.sub a 1 @@ Array.length a - 1

let rtail_n(a: 'a array)(n: int) =
	Array.sub a 0 @@ Array.length a - n

let rtail(a: 'a array): 'a array =
	rtail_n a 1
let last(a: 'a array): 'a =
	Array.get a @@ Array.length a - 1


let output_elements ?(delimeter=", ")(output_element: ('a, 'o) OutputU.printer)(out: 'o OutputU.t)(arr: 'a array): unit =
	iteri arr @@ fun idx em ->
		output_element out em;
		if idx != Array.length arr - 1 then
			OutputU.str out delimeter

(*TODO: Use BatArray.print*)
let output(output: ('a, 'o) OutputU.printer)(out_channel: 'o OutputU.t)(arr: 'a array): unit =
	OutputU.out out_channel "[%a]" (output_elements output) arr

let eq(eq: 'a -> 'b -> bool)(a: 'a array)(b: 'b array): bool =
	same_length a b &&
		not @@ OpU.empty @@ find_zip a b @@ fun a b -> OpU.op_if (eq a b) @@ fun () -> ()
