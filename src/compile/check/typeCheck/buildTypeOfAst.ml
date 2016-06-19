open TypeOfAst

let f(binding: Ast.access -> Binding.t)(rc_asts: Ast.rc array)(un_asts: Ast.un array)(ft_asts: Ast.ft array): t =
	(*TODO: this is repetitive...*)
	let rcs = Rcs.build_from_keys rc_asts begin fun (Ast.Rc(_, rname, _)) ->
		{Type.rname; Type.properties = [||]}
	end in
	let uns = Uns.build_from_keys un_asts begin fun (Ast.Un(_, uname, _)) ->
		{Type.uname; Type.types = [||]}
	end in
	let fts = Fts.build_from_keys ft_asts begin fun (Ast.Ft(_, fname, _)) ->
		{Type.fname; return_type = Type.Void; parameters = [||]}
	end in
	U.returning {rcs; uns; fts} begin fun type_of_ast ->
		Rcs.iter rcs begin fun (Ast.Rc(_, _, properties)) rc ->
			rc.Type.properties <- ArrayU.map properties begin fun (Ast.Property(_, name, typ)) ->
				{Type.prop_name = name; Type.prop_type = TypeCheckU.declared_type binding type_of_ast typ}
			end
		end;
		Uns.iter uns begin fun (Ast.Un(_, _, types)) un ->
			un.Type.types <- ArrayU.map types begin fun typ ->
				(*TODO: duplicate code*)
				TypeCheckU.declared_type binding type_of_ast typ
			end
		end;
		Fts.iter fts begin fun (Ast.Ft(_, _, Ast.Signature(_, return_type_ast, parameter_asts))) ft ->
			ft.Type.return_type <- TypeCheckU.declared_type binding type_of_ast return_type_ast; (*TODO: duplicate code*)
			ft.Type.parameters <- ArrayU.map parameter_asts @@ fun (Ast.Parameter(_, name, typ)) -> name, TypeCheckU.declared_type binding type_of_ast typ
		end
	end
