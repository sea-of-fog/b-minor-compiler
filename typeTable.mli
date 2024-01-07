open Syntax

type 'a res =
    | Ok   of 'a
    | Fail of string

type 'a t

(* classical monad operations *)
val return   : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val run      : 'a t -> 'a res
val fail     : string -> 'a t

val add_to_current_scope : location -> typ -> unit t
val open_scope           : unit t
val close_scope          : unit t

val lookup               : location -> typ t
