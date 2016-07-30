open N.V
open N.Compiler
open N.Run

let create(io: FileIo.t): runtime = {
	current_thread = None;
	thread_queue = Queue.create();
	waiting_threads = MutArray.create();
	compiler = Compiler.create io
}

let compile({compiler; _}: runtime): Path.t -> modul =
	Compiler.compile compiler

let add_thread({thread_queue; _}: runtime)(fn: declared_fn)(args: v array): unit =
	let state = State.create fn args in
	let thread = { state; waiting_on = NotWaiting; waited_on_by = MutArray.create() } in
	Queue.add thread thread_queue

type runtime_state =
	| AllDone of v
	| Working
	| Waiting of Thread.t array

let get_new_thread_helper(runtime: runtime)(done_value: unit -> v): runtime_state =
	let {thread_queue; waiting_threads; _} = runtime in
	match QueueU.try_take thread_queue with
	| Some t ->
		runtime.current_thread <- Some t;
		Working
	| None ->
		if MutArray.empty waiting_threads then
			AllDone(done_value())
		else
			let threads = ArrayU.filter_map (MutArray.to_array waiting_threads) @@ fun thread ->
				match thread.waiting_on with
				| Io thread -> Some thread
				| _ -> None in
			if ArrayU.empty threads then failwith "Circular dependency!";
			Waiting(threads)

(* Returns true if there was a new thread to enqueue. *)
let get_new_thread(runtime: runtime)(v: v): runtime_state =
	get_new_thread_helper runtime @@ fun () -> v

(* Must be Working or Waiting *)
let must_get_new_thread(runtime: runtime): runtime_state =
	get_new_thread_helper runtime @@ fun () -> failwith "boo"

(* Re-enqueue a thread after it gets a value. *)
let requeue_thread(runtime: runtime)(thread: thread)(v: v): unit =
	State.push thread.state v;
	MutArray.remove runtime.waiting_threads thread (==);
	Queue.add thread runtime.thread_queue

(* Returns true there's more to come *)
let step(runtime: runtime): runtime_state =
	let current_thread =
		OpU.or_else runtime.current_thread @@ fun () -> failwith "Tried to step empty state" in
	let {state; waiting_on; waited_on_by} = current_thread in
	assert (waiting_on = NotWaiting);
	match Step.step state with
	| NotDone ->
		Working
	| Done v ->
		MutArray.iter waited_on_by begin fun t ->
			match t.waiting_on with
			| NotWaiting | Io _ ->
				(* If it's not waiting on me, it shouldn't be in my waiting_on *)
				assert false
			| Thread waiting_on ->
				assert (waiting_on == current_thread);
				requeue_thread runtime t v
			| Threads threads ->
				MutArray.remove threads current_thread (==);
				if MutArray.empty threads then
					requeue_thread runtime t v
		end;
		runtime.current_thread <- None;
		get_new_thread runtime v
	| AwaitingIo _ ->
		U.todo();
		(* TODO: Ocaml native threads don't support 'done' callbacks, so
		when the thread is created, it will need to finish with requeueing this thread...*)
		(*Lwt.on_success lwt (requeue_thread runtime current_thread);*)
		MutArray.push runtime.waiting_threads current_thread;
		must_get_new_thread runtime
	| AwaitingThread t ->
		current_thread.waiting_on <- Thread t;
		MutArray.push runtime.waiting_threads current_thread;
		(* Value will never be used. *)
		must_get_new_thread runtime

(*RUNTIME: current_thread, thread_queue, compiler*)
let run(runtime: runtime): v =
	assert (OpU.empty runtime.current_thread);
	runtime.current_thread <- Some(Queue.take runtime.thread_queue);
	U.loop0 @@ fun loop ->
		match step runtime with
		| AllDone v ->
			v
		| Working ->
			loop()
		| Waiting threads ->
			ThreadU.join_all threads;
			loop()
