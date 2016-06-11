(*TODO: Kill and use MapU*)
val iter: ('k, 'v) Hashtbl.t -> ('k -> 'v -> unit) -> unit
val build: (('k -> 'v -> unit) -> unit) -> ('k, 'v) Hashtbl.t
