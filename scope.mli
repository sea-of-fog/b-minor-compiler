(* stores the name (will be used for labeling) of *)
(* the block and the number of local variables *)

type block_data = 
    { label : string;
      local_vars : int }

val resolve : Syntax.prog -> ((block_data, Syntax.location) Syntax.ann_prog) ScopeTable.res
