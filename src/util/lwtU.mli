val choose: 'a Lwt.t array -> 'a Lwt.t
val join: unit Lwt.t array -> unit Lwt.t
val parallel_iter: 'a array -> ('a -> unit Lwt.t) -> unit Lwt.t
val build: (('a -> unit) -> unit Lwt.t) -> 'a array Lwt.t
val all: 'a Lwt.t array -> 'a array Lwt.t
