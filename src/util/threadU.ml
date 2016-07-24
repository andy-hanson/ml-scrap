let join_all(threads: Thread.t array): unit =
	ArrayU.iter threads Thread.join

let do_in_parallel(inputs: 'a array)(action: 'a -> unit): unit =
	join_all @@ ArrayU.map inputs @@ Thread.create @@ fun input ->
		Printexc.print action input
