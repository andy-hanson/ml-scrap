exception EmptyStack
exception CantPeekByN

type 'a t = 'a BatDynArray.t

let create(): 'a t =
	BatDynArray.create()

let empty(gs: 'a t): bool =
	BatDynArray.empty gs

let push(gs: 'a t)(value: 'a): unit =
	BatDynArray.add gs value

let peek(gs: 'a t): 'a =
	if BatDynArray.empty gs then
		raise EmptyStack
	else
		BatDynArray.last gs

let peek_by(gs: 'a t)(n: int): 'a =
	let idx = (BatDynArray.length gs) - 1 - n in
	if idx < 0 then
		raise CantPeekByN
	else
		BatDynArray.get gs idx

let pop(gs: 'a t): 'a =
	let popped = peek gs in
	BatDynArray.delete_last gs;
	popped

let try_pop(gs: 'a t): 'a option =
	U.op_if (not (empty gs)) (fun () -> let last = BatDynArray.last gs in BatDynArray.delete_last gs; last)

let output(out: ('o, 'a) OutputU.printer)(o: 'o BatIO.output)(gs: 'a t) =
	OutputU.out_array out o (BatDynArray.to_array gs)
