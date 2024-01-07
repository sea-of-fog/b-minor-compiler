module type ErrorStateMonad = 
    functor (State: sig type t end) ->
    sig
        type 'a res =
            | Ok   of 'a
            | Fail of string

        type 'a t
        type s = 
            State.t

        val return   : 'a -> 'a t
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

        val fail  : string -> 'a t

        val get : s t
        val set : s -> unit t
        val run : s -> 'a t -> 'a res
    end

module PrivateMake(State : sig type t end) = struct

    type s = 
        State.t 

    type 'a res =
        | Ok   of 'a
        | Fail of string

    type 'a t = 
        s -> ('a res) * s

    let return v = fun s ->
        (Ok v, s)

    let ( let* ) comp f = fun s -> 
        match comp s with
        | (Ok v, s)     -> (f v) s
        | (Fail msg, s) -> (Fail msg, s)

    let fail msg = fun s ->
        (Fail msg, s)

    let get = fun s ->
        (Ok s, s)

    let set new_s = fun s ->
        (Ok (), new_s)

    let run s comp =
        fst @@ comp s

end

module Make = (PrivateMake : ErrorStateMonad)
