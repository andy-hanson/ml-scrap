open N.Ty
open N.TyP


(*TODO:NEATER, MOVE*)
let next_rt_id =
	let nrid = ref 0 in
	fun () ->
		incr nrid;
		!nrid




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

(*TODO:NAME, DOCUMENT BETTER*)
(* Returns a mapped array if *any* invocation of f returns Some. *)
let foo(arr: 'a array)(f: 'a -> 'a option): 'a array option =
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

let rec try_substitute(vars: vars)(ty: ty): ty option =
	U.loop ty @@ fun loop ty ->
		match ty with
		| TPrimitive _ -> None
		| Rt {rt_origin; _} ->
			begin match rt_origin with
			| RtGenInst(gen_rt, old_ty_arguments) ->
				(* TODO:COMMENT *)
				OpU.map (foo old_ty_arguments loop) @@ fun new_ty_arguments ->
					(*TODO: passing in Loc.zero because it should be impossible to have the wrong # of parameters here.
					Should be a neater way to do this.*)
					Rt(instantiate_gen_rt Loc.zero gen_rt new_ty_arguments)
			| _ ->
				None
			end
		| Un _ -> U.todo()
		| Ft _ -> U.todo()
		| GenVar v -> Some(Vars.get vars v)
		| GenRt _ | GenFt _ | GenUn _ -> assert false

(*TODO:DOCUMENT*)
(*TODO: instantiate module*)
and substitute(vars: vars)(ty: ty): ty =
	match try_substitute vars ty with
	| Some t -> t
	| None -> ty

and instantiate_gen_rt(loc: Loc.t)(gen_rt: gen_rt)(tys: ty array): rt =
	(*TODO: we need a check that the rt has been built yet, in case generic types recursively refer to each other.*)
	let { gen_rt_origin = _; gen_rt_params; gen_rt_properties; gen_rt_cache } = gen_rt in
	match N.GenCache.try_get gen_rt_cache tys with
	| Some rt ->
		rt
	| None ->
		U.returning { rt_id = next_rt_id(); rt_origin = RtGenInst(gen_rt, tys); properties = nil_array } @@ fun rt ->
			N.GenCache.set gen_rt_cache tys rt;
			ErrU.check (ArrayU.same_length gen_rt_params tys) loc
				(Err.GenInstParameters(Array.length gen_rt_params, Array.length tys));
			let vars = Vars.build_from_keys_and_values gen_rt_params tys in
			rt.properties <- ArrayU.map gen_rt_properties @@ fun (name, ty_with_vars) ->
				name, substitute vars ty_with_vars

(*TODO: share more code with instantiate_gen_rt*)
(*TODO: share more code with instantiate_gen_rt*)
(*TODO: share more code with instantiate_gen_rt*)
let instantiate_gen_un(loc: Loc.t)(gen_un: gen_un)(tys: ty array): un =
	let { gen_un_origin = _; gen_un_params; gen_un_tys; gen_un_cache } = gen_un in
	match N.GenCache.try_get gen_un_cache tys with
	| Some un -> un
	| None ->
		U.returning { un_origin = UnGenInst(gen_un, tys); utys = nil_array } @@ fun un ->
			N.GenCache.set gen_un_cache tys un;
			ErrU.check (ArrayU.same_length gen_un_params tys) loc
				(Err.GenInstParameters(Array.length gen_un_params, Array.length tys));
			let vars = Vars.build_from_keys_and_values gen_un_params tys in
			un.utys <- ArrayU.map gen_un_tys @@ substitute vars


(*TODO: share more code with instantiate_gen_rt*)
(*TODO: share more code with instantiate_gen_rt*)
(*TODO: share more code with instantiate_gen_rt*)
let instantiate_gen_ft(loc: Loc.t)(gen_ft: gen_ft)(tys: ty array): ft =
	let { gen_ft_origin = _; gen_ft_ty_params; gen_ft_return; gen_ft_parameters; gen_ft_cache } = gen_ft in
	match N.GenCache.try_get gen_ft_cache tys with
	| Some ft -> ft
	| None ->
		U.returning { ft_origin = FtGenInst(gen_ft, tys); return = t_nil; parameters = nil_array } @@ fun ft ->
			N.GenCache.set gen_ft_cache tys ft;
			ErrU.check (ArrayU.same_length gen_ft_ty_params tys) loc
				(Err.GenInstParameters(Array.length gen_ft_ty_params, Array.length tys));
			let vars = Vars.build_from_keys_and_values gen_ft_ty_params tys in
			ft.return <- substitute vars gen_ft_return;
			ft.parameters <- ArrayU.map gen_ft_parameters @@ fun (name, ty) -> name, substitute vars ty

let f(loc: Loc.t)(ty: ty)(args: ty array): ty =
	begin match ty with
	| GenRt g ->
		Rt(instantiate_gen_rt loc g args)
	| GenUn g ->
		Un(instantiate_gen_un loc g args)
	| GenFt g ->
		Ft(instantiate_gen_ft loc g args)
	| _ ->
		U.todo()
	end
