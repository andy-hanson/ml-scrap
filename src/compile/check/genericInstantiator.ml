open N.Ty

module Vars = Lookup.Make(struct
	type t = gen_var
	let equal: t -> t -> bool = (==)
	let hash((loc, _): gen_var): int =
		Loc.hash loc
end)
type vars = ty Vars.t

let instantiate_ty_with_vars(vars: vars): ty_with_vars -> ty = function
	| TVPlain t -> t
	| TVVar var -> Vars.get vars var

let instantiate_gen_rt(loc: Loc.t)(gen_rt: gen_rt)(tys: ty array): rt =
	let { gen_rt_origin = _; gen_rt_params; gen_rt_properties; gen_rt_cache } = gen_rt in
	match N.GenCache.try_get gen_rt_cache tys with
	| Some rt -> rt
	| None ->
		U.returning { rt_origin = RtGenInst(gen_rt, tys); properties = [||] } @@ fun rt ->
			N.GenRtCache.set gen_rt_cache tys rt;
			ErrU.check (ArrayU.same_length gen_rt_params tys) loc
				(Err.GenInstParameters(Array.length gen_rt_params, Array.length tys));
			let vars = Vars.build_from_keys_and_values gen_rt_params tys in
			rt.properties <- ArrayU.map gen_rt_properties @@ fun (name, ty_with_vars) ->
				name, instantiate_ty_with_vars vars ty_with_vars
