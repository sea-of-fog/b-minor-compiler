(* stores the name (will be used for labeling) of *)
(* the block and the number of local variables *)

val resolve : Syntax.prog -> ((Syntax.block_data, Syntax.location) Syntax.ann_prog) ScopeTable.res
