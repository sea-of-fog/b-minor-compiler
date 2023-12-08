type 'a t =
    'a list -> 'a list

(* Constructing lists *)

let empty =
    (fun zs -> zs)

let cons x xs = 
    (fun zs -> x::(xs zs))

let append xs ys =
    (fun zs -> xs (ys zs))

let ( ++ ) xs ys =
    append xs ys

let singleton x =
    (fun zs -> x::zs)

let snoc xs x =
    xs ++ (singleton x)

(* Converting to and from lists *)

let rec of_list xs =
    match xs with
    | []    -> empty
    | x::xs -> cons x (of_list xs)

let prepend xs ys =
    xs ys

let to_list xs =
    prepend xs []
