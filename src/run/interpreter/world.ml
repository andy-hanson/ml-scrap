open N
open BuiltinTyU

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
