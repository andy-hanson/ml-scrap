val declared_type: (Ast.access -> Binding.t) -> TypeOfAst.t -> Ast.typ -> Type.t

val combine_types: Loc.t -> Type.t array -> Type.t
