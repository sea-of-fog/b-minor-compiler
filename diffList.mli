type 'a t

(* Construction *)
val empty     : 'a t
val cons      : 'a -> 'a t -> 'a t
val snoc      : 'a t -> 'a -> 'a t
val singleton : 'a -> 'a t
val append    : 'a t -> 'a t -> 'a t
val ( ++ )    : 'a t -> 'a t -> 'a t

(* Converting between types *)
val of_list  : 'a List.t -> 'a t
val to_list  : 'a t -> 'a List.t
