let forall(s: string)(pred: char -> bool): bool =
	U.loop 0 begin fun i recur ->
		if i = String.length s then
			true
		else if pred @@ String.get s i then
			recur @@ i + 1
		else
			false in
	end
