type t = 
    | Nil
    | Single of string
    | List   of string list
    | Prefix of string * t
    | Suffix of t * string
    | Concat of t * t

let empty =
    Nil 

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

module D = DiffList

let rec to_dlist code =
    match code with
    | Nil              -> D.empty 
    | Single l         -> D.singleton l
    | List ls          -> D.of_list ls
    | Prefix (l, code) -> D.cons l (to_dlist code)
    | Suffix (code, l) -> D.snoc (to_dlist code) l
    | Concat (c1, c2)  -> D.((to_dlist c1) ++ (to_dlist c2))

let to_lines code =
    D.to_list @@ to_dlist code
