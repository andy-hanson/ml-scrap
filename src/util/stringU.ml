let forall(s: string)(pred: char -> bool): bool =
	let rec recur(i: int) =
		if (i = String.length s) then
			true
		else if pred (String.get s i) then
		 	recur (i + 1)
		else
			false in
	recur 0
