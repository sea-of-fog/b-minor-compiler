val check : (Syntax.block_data, Syntax.location) Syntax.ann_prog -> 
    ((Syntax.block_data, Syntax.location * Syntax.typ) Syntax.ann_prog) TypeTable.res
val extract_type : (Syntax.location * Syntax.typ) Syntax.ann_expr -> Syntax.typ
