module type S = sig
	include Map.S
	val try_get: 'v t -> key -> 'v option
	val make: 'a array -> ('a -> key * 'v) -> 'v t
	val overriding_union: 'v t -> 'v t -> 'v t
end

module Make(Ord: Map.OrderedType): S with type key = Ord.t = struct
	include Map.Make(Ord)

	let try_get(map: 'v t)(key: key): 'v option =
		if mem key map then
			Some (find key map)
		else
			None

	let make(arr: 'a array)(fn: 'a -> key * 'v): 'v t =
		ArrayU.fold empty arr begin fun map item ->
			let key, value = fn item in
			add key value map
		end

	let overriding_union(a: 'v t)(b: 'v t): 'v t =
		union (fun _ _ v2 -> Some v2) a b
end
