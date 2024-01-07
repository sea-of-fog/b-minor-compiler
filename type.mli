val check : (Scope.block_data, Syntax.location) Syntax.ann_prog -> 
    ((Scope.block_data, Syntax.location * Syntax.typ) Syntax.ann_prog) TypeTable.res
val extract_type : (Syntax.location * Syntax.typ) Syntax.ann_expr -> Syntax.typ
