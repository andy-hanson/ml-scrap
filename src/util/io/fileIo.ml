exception FileNotFound of Path.t

class type t = object
	(* Throws FileNotFound *)
	method open_in: Path.t -> BatIO.input
	method close_in: BatIO.input -> unit
end
