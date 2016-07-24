type 'a t = 'a Queue.t

let try_take(q: 'a t): 'a option =
	try
		Some(Queue.take q)
	with Queue.Empty ->
		None
