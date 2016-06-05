exception TODO

(* let zip(a: 'a array)(b: 'b array): ('a * 'b) array =
	let f i a =
		(a, Array.get b i) in
	Array.mapi f a *)

let iter_zip(a: 'a array)(b: 'b array)(f: 'a -> 'b -> unit): unit =
	assert (Array.length a = Array.length b);
	for i = 0 to (Array.length a) - 1 do
		f (Array.get a i) (Array.get b i)
	done

let rec loop_until_result(f: unit -> 'a option): 'a =
	match f() with
	| Some res ->
		res
	| None ->
		loop_until_result f

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

let assert_equal(output: 'o BatIO.output -> 'a -> unit)(a: 'a)(b: 'a): unit =
	if not (a = b) then begin
		(* TODO: failwith (OutputU.out_to_string "Expected %a = %a" output a output b) *)
		failwith ("Expected " ^ (OutputU.out_to_string output a) ^ " = " ^ (OutputU.out_to_string output b))
	end

let arr3(arr: 'a array): 'a * 'a * 'a =
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


module MapU(Map: Map.S) = struct
	let x: int = 0

	let make(arr: 'a array)(fn: 'a -> Map.key * 'b): 'b Map.t =
		let add_it map item =
			let key, value = fn item in
			Map.add key value map in
		Array.fold_left add_it Map.empty arr

	let union(a: 'a Map.t)(b: 'a Map.t): 'a Map.t =
		Map.fold Map.add a b
end
