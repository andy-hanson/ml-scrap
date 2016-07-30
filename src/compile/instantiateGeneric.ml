open N.Ty
open N.TyP

(* Returns a mapped array if *any* invocation of f returns Some. *)
let map_if_any_maps(arr: 'a array)(f: 'a -> 'a option): 'a array option =
	let m = MutArray.create() in
	let did_replace = ArrayU.exists arr @@ fun em ->
		match f em with
		| Some x ->
			MutArray.push m x;
			true
		| None ->
			MutArray.push m em;
			false in
	OpU.op_if did_replace @@ fun () -> MutArray.to_array m

module Vars = Lookup.Make(struct
	type t = gen_var
	let equal: t -> t -> bool = N.TyUU.gen_var_equal
	let hash((loc, _): gen_var): int =
		Loc.hash loc
end)
type vars = ty Vars.t

module type Instantiator = sig
	type g
	val instantiate: g -> ty array -> ty
	val instantiate_with_check: g -> Loc.t -> ty array -> ty
end

(*
Necessary functions for making Instantiator instances.
This is the only part that varies between gen_rt, gen_un, and gen_ft.
*)
module type InstantiatorUtils = sig
	type g
	type t
	val stuff: g ->  t gen_stuff
	(* Make an initial instance with dummy data. *)
	val make: g -> ty array -> t
	(* Fill in that initial instance. *)
	val fill: g -> t -> (ty -> ty) -> unit
	val ty_of_t: t -> ty
end

module rec Substitute: sig
	val substitute: vars -> ty -> ty
end = struct
	let try_substitute(vars: vars)(ty: ty): ty option =
		U.loop ty @@ fun loop ty ->
			let try_replace(old_ty_arguments: ty array)(replace: ty array -> 't): 't option =
				OpU.map (map_if_any_maps old_ty_arguments loop) replace in
			match ty with
			| TPrimitive _ ->
				None
			| Rt {rt_origin; _} ->
				begin match rt_origin with
				| RtGenInst(g, tys) -> try_replace tys @@ RtInstantiator.instantiate g
				| _ -> None
				end
			| Un {un_origin; _} ->
				begin match un_origin with
				| UnGenInst(g, tys) -> try_replace tys @@ UnInstantiator.instantiate g
				| _ -> None
				end
			| Ft {ft_origin; _} ->
				begin match ft_origin with
				| FtGenInst(g, tys) -> try_replace tys @@ FtInstantiator.instantiate g
				| _ -> None
				end
			| GenVar v ->
				Some(Vars.get vars v)
			| GenRt _ | GenFt _ | GenUn _ ->
				assert false

	let substitute(vars: vars)(ty: ty): ty =
		match try_substitute vars ty with
		| Some t -> t
		| None -> ty
end

and MakeInstantiator: functor(I: InstantiatorUtils) -> Instantiator with type g = I.g = functor(I: InstantiatorUtils) -> struct
	type g = I.g

	let instantiate(g: I.g)(tys: ty array): ty =
		let {gen_params; gen_cache} = I.stuff g in
		I.ty_of_t @@ OpU.or_else (N.GenCache.try_get gen_cache tys) @@ fun () ->
			U.returning (I.make g tys) @@ fun made ->
				N.GenCache.set gen_cache tys made;
				let vars = Vars.build_from_keys_and_values gen_params tys in
				I.fill g made @@ Substitute.substitute vars

	let instantiate_with_check(g: I.g)(loc: Loc.t)(tys: ty array): ty =
		let {gen_params; _} = I.stuff g in
		ErrU.check (ArrayU.same_length gen_params tys) loc (Err.GenInstParameters(Array.length gen_params, Array.length tys));
		instantiate g tys
end

and RtInstantiator: Instantiator with type g = gen_rt = MakeInstantiator(struct
	type g = gen_rt
	type t = rt
	let stuff({gen_rt_stuff; _}: g) = gen_rt_stuff
	let make(gen_rt: g)(tys: ty array): t = { rt_origin = RtGenInst(gen_rt, tys); properties = ArrayU.nil }
	let fill({gen_rt_properties; _}: g)(rt: rt)(substitute: ty -> ty): unit =
		rt.properties <- ArrayU.map gen_rt_properties @@ fun (name, ty) -> name, substitute ty
	let ty_of_t(rt: t): ty = Rt rt
end)

and UnInstantiator: Instantiator with type g = gen_un = MakeInstantiator(struct
	type g = gen_un
	type t = un
	let stuff({gen_un_stuff; _}) = gen_un_stuff
	let make(gen_un: g)(tys: ty array): t = { un_origin = UnGenInst(gen_un, tys); utys = ArrayU.nil }
	let fill({gen_un_tys; _}: g)(un: t)(substitute: ty -> ty): unit =
		un.utys <- ArrayU.map gen_un_tys substitute
	let ty_of_t(un: t): ty = Un un
end)

and FtInstantiator: Instantiator with type g = gen_ft = MakeInstantiator(struct
	type g = gen_ft
	type t = ft
	let stuff({gen_ft_stuff; _}) = gen_ft_stuff
	let make(gen_ft: g)(tys: ty array): t = { ft_origin = FtGenInst(gen_ft, tys); return = t_nil; parameters = ArrayU.nil }
	let fill({gen_ft_return; gen_ft_parameters; _}: g)(ft: t)(substitute: ty -> ty): unit =
		ft.return <- substitute gen_ft_return;
		ft.parameters <- ArrayU.map gen_ft_parameters @@ fun (name, ty) -> name, substitute ty
	let ty_of_t(ft: t): ty = Ft ft
end)

let f(loc: Loc.t)(ty: ty)(args: ty array): ty =
	let instantiator =
		match ty with
		| GenRt g -> RtInstantiator.instantiate_with_check g
		| GenUn g -> UnInstantiator.instantiate_with_check g
		| GenFt g -> FtInstantiator.instantiate_with_check g
		| _ -> U.todo() in
	instantiator loc args
