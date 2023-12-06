type t = string list

let empty = []

let single_line s = 
    [s]

let add_line line code =
    code@[line]

let concat c1 c2 = c1@c2

let prefix line lines =
    line::lines

let from_list lines =
    lines

let to_lines code = code
