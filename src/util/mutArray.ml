type 'a t = 'a BatDynArray.t

let create(): 'a t =
	BatDynArray.create()

let get = BatDynArray.get
let last = BatDynArray.last
let length = BatDynArray.length
let empty = BatDynArray.empty

let to_array = BatDynArray.to_array

let set = BatDynArray.set
let add = BatDynArray.add
let delete_last = BatDynArray.delete_last
let delete_range = BatDynArray.delete_range

let iter(a: 'a t)(f: 'a -> unit): unit =
	BatDynArray.iter f a


let slice(a: 'a t)(start: int)(length: int): 'a array =
	Array.init length (fun i -> get a (start + i))

let tail(a: 'a t): 'a array =
	slice a 1 (length a - 1)
