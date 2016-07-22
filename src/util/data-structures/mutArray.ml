exception Empty

type 'a t = 'a BatDynArray.t

let create(): 'a t =
	BatDynArray.create()
let of_array = BatDynArray.of_array

let get = BatDynArray.get
let length = BatDynArray.length
let empty = BatDynArray.empty

let to_array = BatDynArray.to_array

let blit = BatDynArray.blit
let set = BatDynArray.set
let push = BatDynArray.add
let push_many(a: 'a t)(values: 'a array): unit =
	Array.iter (push a) values
let delete_last = BatDynArray.delete_last
let delete_range = BatDynArray.delete_range
let delete_last_n(a: 'a t)(n: int): unit =
	delete_range a (length a - n) n

let peek(a: 'a t): 'a =
	if empty a then
		raise Empty
	else
		BatDynArray.last a

let pop(a: 'a t): 'a =
	U.returning (peek a) @@ fun _ -> delete_last a


let iter(a: 'a t)(f: 'a -> unit): unit =
	BatDynArray.iter f a


let slice(a: 'a t)(start: int)(length: int): 'a array =
	Array.init length @@ fun i -> get a (start + i)

let tail(a: 'a t): 'a array =
	slice a 1 @@ length a - 1

let map_to_array(a: 'a t)(f: 'a -> 'b): 'b array =
	to_array @@ BatDynArray.map f a

let pop_n(a: 'a t)(n: int): 'a array =
	let start = (length a) - n in
	U.returning (slice a start n) begin fun _ ->
		delete_range a start n
	end


let un_let(a: 'a t)(n: int): unit =
	let l = length a in
	set a (l - n - 1) (peek a);
	delete_last_n a n

let output_with_max(max: int)(output_element: ('a, 'o) OutputU.printer)(out: 'o OutputU.t)(a: 'a t): unit =
	let n = length a in
	if n <= max then
		ArrayU.output output_element out @@ to_array a
	else
		let tail = slice a (n - max) max in
		OutputU.out out "[... %a]"
			(ArrayU.output_elements output_element) tail