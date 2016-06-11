module type Key = sig
	(*TODO: this is just Hashtbl.Hashable*)
	type t
	val equal: t -> t -> bool
	val hash: t -> int
end

module type S = sig
	type key
	type 'v t

	val create: unit -> 'v t
	val create_with_size: int -> 'v t
	val build: ((key -> 'v -> unit) -> unit) -> 'v t
	val build_from_keys: key array -> (key -> 'v) -> 'v t
	val build_from_values: 'v array -> ('v -> key) -> 'v t

	val set: 'v t -> key -> 'v -> unit
	val get: 'v t -> key -> 'v
	val get_or_update: 'v t -> key -> (unit -> 'v) -> 'v
	val try_get: 'v t -> key -> 'v option

	val keys: 'v t -> key array
	val values: 'v t -> 'v array
	val iter: 'v t -> (key -> 'v -> unit) -> unit
	val output: ((key, 'o) OutputU.printer) -> (('v, 'o) OutputU.printer) -> ('o OutputU.t) -> 'v t -> unit
end

module Make(K: Key): S with type key = K.t = struct
	module H = Hashtbl.Make(K)

	type key = K.t
	type 'v t = 'v H.t

	let create(): 'v t =
		H.create 0
	let create_with_size = H.create

	let build(builder: (key -> 'v -> unit) -> unit): 'v t =
		U.returning (create()) (fun m -> builder (H.add m))

	let build_from_keys(keys: key array)(get_value: key -> 'v): 'v t =
		U.returning (create_with_size (Array.length keys)) begin fun m ->
			ArrayU.iter keys begin fun key ->
				H.add m key (get_value key)
			end
		end

	let build_from_values(values: 'v array)(get_key: 'v -> key): 'v t =
		U.returning (create_with_size (Array.length values)) begin fun m ->
			ArrayU.iter values begin fun v ->
				H.add m (get_key v) v
			end
		end

	let set = H.add
	(*TODO: return an option*)
	let get = H.find
	let get_or_update(t: 'v t)(key: key)(get_value: unit -> 'v): 'v =
		try
			get t key
		with Not_found ->
			U.returning (get_value()) (set t key)
	let try_get m key =
		try
			Some (get m key)
		with
			Not_found -> None

	let keys m =
		ArrayU.build begin fun build ->
			(*TODO: hashU.iter*)
			H.iter (fun k _ -> build k) m
		end

	let values m =
		ArrayU.build begin fun build ->
			H.iter (fun _ v -> build v) m
		end

	let iter(tbl: 'v t)(f: ('k -> 'v -> unit)): unit =
		H.iter f tbl

	let output out_key out_val out m =
		(*TODO: copied from out_hashtbl*)
		OutputU.str out "{ ";
		iter m begin fun key value ->
			out_key out key;
			OutputU.str out ": ";
			out_val out value;
			OutputU.str out ", "
		end;
		OutputU.str out "}"
end
