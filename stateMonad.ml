module type StateFunctor = 
    functor (State: sig type t end) ->
    sig
        type 'a t
        type s = 
            State.t

        val return   : 'a -> 'a t
        val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

        val get : s t
        val set : s -> unit t
        val run : s -> 'a t -> 'a
    end

module PrivateMake(State : sig type t end) = struct

    type s = 
        State.t 

    type 'a t = 
        s -> ('a * s)

    let return v = fun s ->
        (v, s)

    let ( let* ) comp f = fun s -> 
        let (v, s) = comp s in
            (f v) s

    let get = fun s ->
        (s, s)

    let set new_s = fun s ->
        ((), new_s)

    let run s comp =
        fst @@ comp s

end

module Make = (PrivateMake : StateFunctor)
