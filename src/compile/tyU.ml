open N.Ty

let rt_name({rt_origin; _}: rt) =
	begin match rt_origin with
	| RtBuiltin name -> name
	| RtDecl(_, name, _) -> name
	| RtGenInst _ -> U.todo()
	end

(*TODO:KILL?*)
let name = function
	| TPrimitive p ->
		Sym.of_string begin match p with
		| TBool -> "Bool"
		| TFloat -> "Float"
		| TInt -> "Int"
		| TString -> "String"
		| TVoid -> "Void"
		| TNil -> "Nil"
		end
	| Rt r -> rt_name r
	| Un {uname; _} -> uname
	| Ft {ft_origin; _} ->
		begin match ft_origin with
		| FtBuiltin name -> name
		| FtDecl(_, name, _) -> name
		| FtFromFn _ -> U.todo()
		| FtFromRt rt -> rt_name rt
		| FtFromPartial _ -> U.todo()
		| FtGenInst _ -> U.todo()
		end
	| GenRt {gen_rt_origin = (_, name, _, _); _} -> name
	| GenFt _ -> U.todo()
	| GenVar _ -> U.todo()

let ft_or_gen_arity(f: ft_or_gen): int =
	Array.length @@	match f with
		| FoG_Ft {parameters; _} -> parameters
		| FoG_Gen {gen_ft_parameters; _} -> gen_ft_parameters

let rt_arity({properties; _}: rt): int =
	Array.length properties

let partial({ft_origin = _; return; parameters} as ft: ft)(num_partial_args: int): ft =
	let parameters = ArrayU.rtail_n parameters num_partial_args in
	{ft_origin = FtFromPartial(ft); return; parameters}

let ty_of_ft_or_gen: ft_or_gen -> ty = function
	| FoG_Ft f -> Ft f
	| FoG_Gen g -> GenFt g
