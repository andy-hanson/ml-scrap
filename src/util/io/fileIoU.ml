let read(io: FileIo.t)(path: Path.t)(f: BatIO.input -> 'a): 'a =
	let input = io#open_in path in
	U.returning (f input) @@ fun _ -> io#close_in input
