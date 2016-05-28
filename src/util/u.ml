exception TODO

(* let zip(a: 'a array)(b: 'b array): ('a * 'b) array =
	let f i a =
		(a, Array.get b i) in
	Array.mapi f a *)

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

let build_array(f: unit -> 'a option): 'a array =
	let arr = BatDynArray.create() in
	let rec recur() =
		match f() with
		| Some x ->
			BatDynArray.add arr x;
			recur()
		| None -> () in
	recur();
	BatDynArray.to_array arr

(*TODO:RENAME*)
let build_array_2(f: unit -> 'a * bool): 'a array =
	let arr = BatDynArray.create() in
	let rec recur() =
		let (x, continue) = f() in
		BatDynArray.add arr x;
		if continue then recur() in
	recur();
	BatDynArray.to_array arr


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
