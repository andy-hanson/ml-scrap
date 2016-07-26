let combine = (+)
let combine_n(hashables: 'a array)(hash: 'a -> int): int =
	ArrayU.fold 0 hashables @@ fun h hashable ->
		combine h @@ hash hashable
