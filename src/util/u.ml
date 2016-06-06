(*TODO:MLI*)

exception TODO

let do_times(times: int)(action: 'a -> unit): 'a =
	for i = 1 to times do
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

let assert_equal(output: 'o OutputU.t -> 'a -> unit)(a: 'a)(b: 'a): unit =
	if not (a = b) then begin
		(* TODO: failwith (OutputU.out_to_string "Expected %a = %a" output a output b) *)
		failwith ("Expected " ^ (OutputU.out_to_string output a) ^ " = " ^ (OutputU.out_to_string output b))
	end

(*TODO: its own file*)
module MapU(Map: Map.S) = struct
	let x: int = 0

	let make(arr: 'a array)(fn: 'a -> Map.key * 'b): 'b Map.t =
		ArrayU.fold Map.empty arr begin fun map item ->
			let key, value = fn item in
			Map.add key value map
		end

	let union(a: 'a Map.t)(b: 'a Map.t): 'a Map.t =
		Map.fold Map.add a b
end
