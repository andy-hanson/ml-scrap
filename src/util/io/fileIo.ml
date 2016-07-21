exception FileNotFound of Path.t

class type t = object
	method read: Path.t -> string Lwt.t
end
