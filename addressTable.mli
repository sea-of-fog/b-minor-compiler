(** Monad operations *)
type 'a t
val run      : 'a t -> 'a
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

(** Free a memory location. Local stack space for
    variables is not changed, only temps and registers
    are actually freed *)
val free        : Syntax.memory   -> unit t

(** Allocate temporary space as either a register or
    stack space *)
val alloc       : Syntax.location -> Syntax.memory t

(** Make all the necessary changes to deal with scopes *)
val open_scope  : Syntax.block_data -> unit t

(** Close the current scope and return correct block data 
    (with included number of temps) *)
val close_scope : Syntax.block_data t
