module Make(Map: Map.S): sig
	val make: 'a array -> ('a -> Map.key * 'b) -> 'b Map.t
	val union: 'a Map.t -> 'a Map.t -> 'a Map.t
end = struct
	let make(arr: 'a array)(fn: 'a -> Map.key * 'b): 'b Map.t =
		ArrayU.fold Map.empty arr begin fun map item ->
			let key, value = fn item in
			Map.add key value map
		end

	let union(a: 'a Map.t)(b: 'a Map.t): 'a Map.t =
		Map.fold Map.add a b
end
