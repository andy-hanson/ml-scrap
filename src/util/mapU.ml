module type S = sig
	include Map.S
	val map_values: 'v t -> ('v -> 'u) -> 'u t
	val build: ((key -> 'v -> unit) -> unit) -> 'v t
	val try_get: 'v t -> key -> 'v option
	val make: 'a array -> ('a -> key * 'v) -> 'v t
	val overriding_union: 'v t -> 'v t -> 'v t
end

module Make(Ord: Map.OrderedType): S with type key = Ord.t = struct
	include Map.Make(Ord)

	let map_values(m: 'v t)(f: 'v -> 'u): 'u t =
		map f m

	let build(build: (key -> 'v -> unit) -> unit): 'v t =
		let m = ref empty in
		build (fun k v -> m := add k v !m);
		!m

	let try_get(map: 'v t)(key: key): 'v option =
		if mem key map then
			Some (find key map)
		else
			None

	let make(arr: 'a array)(fn: 'a -> key * 'v): 'v t =
		build begin fun build ->
			ArrayU.iter arr begin fun item ->
				let k, v = fn item in
				build k v
			end
		end

	let overriding_union(a: 'v t)(b: 'v t): 'v t =
		union (fun _ _ v2 -> Some v2) a b
end
