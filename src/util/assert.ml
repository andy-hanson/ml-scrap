let equal(a: 'a)(b: 'a)(output: ('a, 'o) OutputU.printer): unit =
	if not (a = b) then
		failwith @@ OutputU.out_to_string "Expected %a = %a" output a output b
