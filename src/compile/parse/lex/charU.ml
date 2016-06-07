let in_range(ch: char)(min: char)(max: char): bool =
	min <= ch && ch <= max

let is_digit(ch: char): bool =
	in_range ch '0' '9'

let is_name_char(ch: char): bool =
	in_range ch 'a' 'z' ||
	in_range ch 'A' 'Z' ||
	match ch with
	| '+' | '*' | '/' | '^' | '?' | '<' | '>' ->
		true
	| _ ->
		false
