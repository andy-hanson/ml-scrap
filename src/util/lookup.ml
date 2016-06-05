module type LookupKey = sig
    type t
end

module T(Key: LookupKey): sig
	type 'v t

	val create: unit -> 'v t
	val set: 'v t -> Key.t -> 'v -> unit
	val get: 'v t -> Key.t -> 'v
	val keys: 'v t -> Key.t array
	val values: 'v t -> 'v array

end = struct
	module H = Hashtbl.Make(struct
		type t = Key.t
		let equal = (==)
		let hash = Hashtbl.hash
	end)

	type 'v t = 'v H.t

	let create(): 'v t =
		H.create 0

	let set m k v =
		H.add m k v

	let get m k =
		H.find m k

	let keys m =
		U.build_array_0 (fun build -> H.iter (fun k _ -> build k) m)

	let values m =
		U.build_array_0 (fun build -> H.iter (fun _ v -> build v) m)
end
