type t = 
    | Nil
    | Single of string
    | List   of string list
    | Prefix of string * t
    | Suffix of t * string
    | Concat of t * t

let empty = Nil 

let single_line s = 
    Single s 

let add_line line code =
    Suffix (code, line)

let concat c1 c2 = 
    Concat (c1, c2)

let prefix line code =
    Prefix (line, code)

let from_list lines =
    List lines

let rec append_cps xs ys k =
    match xs with
    | []     -> k ys
    | x::xs  -> append_cps xs ys (fun res -> k (x::res))

let rec suffix_cps xs x k =
    match xs with
    | []    -> x::[]
    | r::rs -> suffix_cps rs x (fun res -> k (r::res))

let rec to_lines_cps code k =
    match code with
    | Nil             -> k []
    | Single l        -> k [l]
    | List ls         -> k ls
    | Prefix (l, ls)  -> to_lines_cps ls 
                             (fun res -> k (l::res))
    | Suffix (ls, l)  -> to_lines_cps ls 
                             (fun res -> suffix_cps res l k)
    | Concat (l1, l2) -> to_lines_cps l1 (fun l1 -> 
                             to_lines_cps l2 (fun l2 ->
                                 append_cps l1 l2 k))

let to_lines code = 
    to_lines_cps code (fun x -> x)
