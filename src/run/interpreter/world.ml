open N
open BuiltinTypeU

(*TODO: ensure that types match up with values, e.g. print type to print value*)

let print = ft1 "Print" t_void "printed" t_string
let ty = rc_n "World" [|
	"print", print(*;
	"read-line", ft0 "read-line" t_string*)
|]
(*TODO:BETTER*)
let rt_world = match ty with | Rt rt -> rt | _ -> assert false
let ft_print = match print with | Ft ft -> ft | _ -> assert false

let builtin(ft: ft)(exec: interpreter_state -> unit): v =
	Fn(BuiltinFn {
		builtin_fn_ty = ft;
		exec
	})

let world = Rc(rt_world, [|
	builtin ft_print @@ fun state ->
		let v = State.pop state in
		OutputU.printf "%a\n" ValU.output v;
		State.push state v_void
|])

let tys = [| ty; print |]

(*
let print = rc1 "Print" "value" Any
(*TODO use a singleton (N.tySn)*)
let read_line = rc1 "ReadLine" "value" t_void

let ct_world = Ct {
	cname = Sym.of_string "World";
	ct_cases = [|
		t_void, print;
		t_string, read_line
	|]
}
let ty_world = TFn ct_world

let tys = [|
	print; read_line;
	ty_world
|]

module RtLookup = Lookup.Make(struct
	type t = N.rt
	let equal = (==)
	let hash({N.rname; _}: t) = Sym.hash rname
end)

type handler =
	| Fn1 of (N.v -> N.v)
	(*| Fn2 of (N.v -> N.v -> N.v)
	| Fn3 of (N.v -> N.v -> N.v -> N.v)*)

let message_handlers: handler RtLookup.t =
	let extract_rc(t: N.ty): N.rt =
		match t with
		| N.Rt r -> r
		| _ -> assert false in

	RtLookup.build begin fun build ->
		let add1(ty: ty)(f: v -> v): unit =
			build (extract_rc ty) (Fn1 f) in
		add1 print @@ fun v ->
			OutputU.printf "%a\n" ValU.output v;
			N.v_void
	end

let call(msg: N.v): N.v =
	match msg with
	| N.Primitive _ | N.Fn _ ->
		assert false
	| N.Rc(ty, properties) ->
		let handler = RtLookup.get message_handlers ty in
		begin match handler with
		| Fn1 f ->
			f @@ ArrayU.single_of properties
		(*| Fn2 f ->
			let a, b = ArrayU.pair_of properties in
			f a b
		| Fn3 f ->
			let a, b, c = ArrayU.triple_of properties in
			f a b c*)
		end

(*TODO: helper for this?*)
let world = N.Fn(N.BuiltinFn {
	N.builtin_ty_fn = ct_world;
	N.exec = fun state -> State.push state @@ call @@ State.pop state
})
*)
