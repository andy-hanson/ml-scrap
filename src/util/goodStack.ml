exception EmptyStack

type 'a t = 'a BatDynArray.t

let create(): 'a t =
	BatDynArray.create()

let get(gs: 'a t)(n: int): 'a =
	(*TODO:proper error*)
	BatDynArray.get gs n

let size(gs: 'a t): int =
	BatDynArray.length gs

let empty(gs: 'a t): bool =
	BatDynArray.empty gs

let push(gs: 'a t)(value: 'a): unit =
	BatDynArray.add gs value

let peek(gs: 'a t): 'a =
	if BatDynArray.empty gs then
		raise EmptyStack
	else
		BatDynArray.last gs

let pop(gs: 'a t): 'a =
	let popped = peek gs in
	BatDynArray.delete_last gs;
	popped

let try_pop(gs: 'a t): 'a option =
	U.op_if (not (empty gs)) (fun () -> let last = BatDynArray.last gs in BatDynArray.delete_last gs; last)

let output(out: ('o, 'a) OutputU.printer)(o: 'o OutputU.t)(gs: 'a t) =
	OutputU.out_array out o (BatDynArray.to_array gs)
