module type Key = Hashtbl.HashedType

module type S = sig
	type key
	type 'v t

	val create: unit -> 'v t
	val create_with_size: int -> 'v t
	val build: ((key -> 'v -> unit) -> unit) -> 'v t
	val build_from_keys: key array -> (key -> 'v) -> 'v t
	val build_from_keys_with_index: key array -> (int -> key -> 'v) -> 'v t
	val build_from_values: 'v array -> ('v -> key) -> 'v t

	val size: 'v t -> int

	val set: 'v t -> key -> 'v -> unit
	val get: 'v t -> key -> 'v
	val get_or_update: 'v t -> key -> (unit -> 'v) -> 'v
	val try_get: 'v t -> key -> 'v option
	val has_key: 'v t -> key -> bool

	val iter: 'v t -> (key -> 'v -> unit) -> unit
	val iter_keys: 'v t -> (key -> unit) -> unit
	val iter_values: 'v t -> ('v -> unit) -> unit
	val keys: 'v t -> key array
	val values: 'v t -> 'v array
	val output: ((key, 'o) OutputU.printer) -> (('v, 'o) OutputU.printer) -> ('v t, 'o) OutputU.printer
end

module Make(K: Key): S with type key = K.t = struct
	(*TODO: type annotations in here*)
	module H = Hashtbl.Make(K)

	type key = K.t
	type 'v t = 'v H.t

	let create(): 'v t =
		H.create 0
	let create_with_size = H.create

	let build(builder: (key -> 'v -> unit) -> unit): 'v t =
		U.returning (create()) @@ fun m -> builder @@ H.add m

	let build_from_keys_with_index(keys: key array)(get_value: int -> key -> 'v): 'v t =
		U.returning (create_with_size @@ Array.length keys) begin fun m ->
			ArrayU.iteri keys begin fun i key ->
				H.add m key @@ get_value i key
			end
		end

	let build_from_keys keys get_value =
		build_from_keys_with_index keys @@ fun _ key -> get_value key

	let build_from_values(values: 'v array)(get_key: 'v -> key): 'v t =
		U.returning (create_with_size @@ Array.length values) begin fun m ->
			ArrayU.iter values begin fun v ->
				H.add m (get_key v) v
			end
		end

	let size = H.length

	let set = H.add
	let get = H.find
	let get_or_update(t: 'v t)(key: key)(get_value: unit -> 'v): 'v =
		try
			get t key
		with Not_found ->
			U.returning (get_value()) @@ set t key
	let try_get m key =
		try
			Some (get m key)
		with
			Not_found -> None
	let has_key m key =
		H.mem m key

	let iter(tbl: 'v t)(f: key -> 'v -> unit): unit =
		H.iter f tbl

	let iter_keys(tbl: 'v t)(f: key -> unit): unit =
		iter tbl @@ fun k _ -> f k

	let iter_values(tbl: 'v t)(f: 'v -> unit): unit =
		iter tbl @@ fun _ v -> f v

	let iteri(tbl: 'v t)(f: (int -> 'k -> 'v -> unit)): unit =
		let i = ref 0 in
		iter tbl begin fun k v ->
			f !i k v;
			incr i
		end

	let keys m =
		ArrayU.build (iter_keys m)

	let values m =
		ArrayU.build (iter_values m)

	let output out_key out_val out m =
		let last_i = size m - 1 in
		OutputU.str out "{";
		iteri m begin fun i key value ->
			out_key out key;
			OutputU.str out ": ";
			out_val out value;
			if i != last_i then
				OutputU.str out ", "
		end;
		OutputU.str out "}"
end

module Str: S with type key = string = Make(struct
	include String
	let hash = Hashtbl.hash
end)
