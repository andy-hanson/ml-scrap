(*TODO: kill and just use MutArray*)

type 'a t = 'a MutArray.t

let create(): 'a t =
	MutArray.create()

let of_array(arr: 'a array) =
	MutArray.of_array arr

let get(gs: 'a t)(n: int): 'a =
	MutArray.get gs n

let size(gs: 'a t): int =
	MutArray.length gs

let empty(gs: 'a t): bool =
	MutArray.empty gs

let push(gs: 'a t)(value: 'a): unit =
	MutArray.add gs value

let push_many(gs: 'a t)(values: 'a array): unit =
	ArrayU.iter values @@ push gs

let peek: 'a t -> 'a = MutArray.peek
let pop: 'a t -> 'a = MutArray.pop

let pop_n(gs: 'a t)(n: int): 'a array =
	let start = (size gs) - n in
	U.returning (MutArray.slice gs start n) begin fun _ ->
		MutArray.delete_range gs start n
	end

let un_let(gs: 'a t)(n: int): unit =
	let l = MutArray.length gs in
	MutArray.set gs (l - n - 1) (MutArray.peek gs);
	MutArray.delete_last_n gs n

let output_with_max(max: int)(output_element: ('a, 'o) OutputU.printer)(out: 'o OutputU.t)(gs: 'a t) =
	let n = size gs in
	if n <= max then
		OutputU.out_array output_element out @@ MutArray.to_array gs
	else
		let tail = MutArray.slice gs (n - max) max in
		OutputU.out out "[... %a]"
			(OutputU.out_array_elements output_element) tail
