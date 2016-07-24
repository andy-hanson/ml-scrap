let choose(threads: 'a Lwt.t array): 'a Lwt.t =
	Lwt.choose @@ Array.to_list threads

let join(threads: unit Lwt.t array): unit Lwt.t =
	Lwt.join @@ Array.to_list threads

let parallel_iter(inputs: 'a array)(action: 'a -> unit Lwt.t): unit Lwt.t =
	Lwt.join @@ ArrayU.map_to_list inputs action

let build(builder: ('a -> unit) -> unit Lwt.t): 'a array Lwt.t =
	let out: 'a MutArray.t = MutArray.create() in
	let%lwt () = builder @@ MutArray.push out in
	Lwt.return @@ MutArray.to_array out

(* Output may be in any order. *)
let all(threads: 'a Lwt.t array): 'a array Lwt.t =
	build @@ fun build ->
		parallel_iter threads @@ Lwt.map build
