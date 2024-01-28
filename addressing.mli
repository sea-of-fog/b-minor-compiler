val generate_addresses : (Syntax.block_data, Syntax.location * Syntax.typ) Syntax.ann_prog -> 
                        (Syntax.block_data_v2, Syntax.memory * Syntax.typ) Syntax.ann_prog

val extract_data     :  'a Syntax.ann_expr -> 'a
val extract_location :  ('a * 'b) Syntax.ann_expr -> 'a
