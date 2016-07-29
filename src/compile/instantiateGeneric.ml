open N.Ty
open N.TyP

(* Use this for a temporary array value to overwrite later. *)
(*TODO: duplicate definition in typeOfAst.ml*)
let nil_array = [||]

module Vars = Lookup.Make(struct
	type t = gen_var
	let equal: t -> t -> bool = N.TyUU.gen_var_equal
	let hash((loc, _): gen_var): int =
		Loc.hash loc
end)
type vars = ty Vars.t

module type Instantiator = sig
	type g
	type t
	val instantiate: g -> Loc.t -> ty array -> t
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
end

module rec Substitute: sig
	val substitute: vars -> ty -> ty
end = struct
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

	let try_substitute(vars: vars)(ty: ty): ty option =
		U.loop ty @@ fun loop ty ->
			match ty with
			| TPrimitive _ -> None
			| Rt {rt_origin; _} ->
				begin match rt_origin with
				| RtGenInst(gen_rt, old_ty_arguments) ->
					(* TODO:COMMENT *)
					OpU.map (map_if_any_maps old_ty_arguments loop) @@ fun new_ty_arguments ->
						(*TODO: passing in Loc.zero because it should be impossible to have the wrong # of parameters here.
						Should be a neater way to do this.*)
						Rt(RtInstantiator.instantiate gen_rt Loc.zero new_ty_arguments)
				| _ ->
					None
				end
			| Un _ -> U.todo()
			| Ft _ -> U.todo()
			| GenVar v -> Some(Vars.get vars v)
			| GenRt _ | GenFt _ | GenUn _ -> assert false

	(*TODO:DOCUMENT*)
	(*TODO: instantiate module*)
	let substitute(vars: vars)(ty: ty): ty =
		match try_substitute vars ty with
		| Some t -> t
		| None -> ty
end

and MakeInstantiator: functor(I: InstantiatorUtils) -> Instantiator with type g = I.g with type t = I.t =
	functor(I: InstantiatorUtils) -> struct
	type g = I.g
	type t = I.t
	let instantiate(g: I.g)(loc: Loc.t)(tys: ty array): I.t =
		(*TODO: dont' always need the check...*)
		let {gen_params; gen_cache} = I.stuff g in
		ErrU.check (ArrayU.same_length gen_params tys) loc (Err.GenInstParameters(Array.length gen_params, Array.length tys));
		OpU.or_else (N.GenCache.try_get gen_cache tys) @@ fun () ->
			U.returning (I.make g tys) @@ fun made ->
				N.GenCache.set gen_cache tys made;
				let vars = Vars.build_from_keys_and_values gen_params tys in
				I.fill g made @@ Substitute.substitute vars
end

and RtInstantiator: Instantiator with type g = gen_rt with type t = rt = MakeInstantiator(struct
	type g = gen_rt
	type t = rt
	let stuff({gen_rt_stuff; _}: g) = gen_rt_stuff
	let make(gen_rt: g)(tys: ty array): t = { rt_origin = RtGenInst(gen_rt, tys); properties = nil_array }
	let fill({gen_rt_properties; _}: g)(rt: rt)(substitute: ty -> ty): unit =
		rt.properties <- ArrayU.map gen_rt_properties @@ fun (name, ty) -> name, substitute ty
end)
and UnInstantiator: Instantiator with type g = gen_un with type t = un = MakeInstantiator(struct
	type g = gen_un
	type t = un
	let stuff({gen_un_stuff; _}) = gen_un_stuff
	let make(gen_un: g)(tys: ty array): t = { un_origin = UnGenInst(gen_un, tys); utys = nil_array }
	let fill({gen_un_tys; _}: g)(un: t)(substitute: ty -> ty): unit =
		un.utys <- ArrayU.map gen_un_tys substitute
end)
and FtInstantiator: Instantiator with type g = gen_ft with type t = ft = MakeInstantiator(struct
	type g = gen_ft
	type t = ft
	let stuff({gen_ft_stuff; _}) = gen_ft_stuff
	let make(gen_ft: g)(tys: ty array): t = { ft_origin = FtGenInst(gen_ft, tys); return = t_nil; parameters = nil_array }
	let fill({gen_ft_return; gen_ft_parameters; _}: g)(ft: t)(substitute: ty -> ty): unit =
		ft.return <- substitute gen_ft_return;
		ft.parameters <- ArrayU.map gen_ft_parameters @@ fun (name, ty) -> name, substitute ty
end)

let f(loc: Loc.t)(ty: ty)(args: ty array): ty =
	begin match ty with
	| GenRt g ->
		Rt(RtInstantiator.instantiate g loc args)
	| GenUn g ->
		Un(UnInstantiator.instantiate g loc args)
	| GenFt g ->
		Ft(FtInstantiator.instantiate g loc args)
	| _ ->
		U.todo()
	end
