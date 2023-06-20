type op = Add | Sub | Mul | Div

type token =
    | Number of int
    | Op of op
    | OpenParen
    | ClosedParen

let digits = [0;1;2;3;4;5;6;7;8;9]
let digit_strings = List.map string_of_int digits

let suffix str = (String.sub str 1 (String.length str - 1))

let digit_lex str = if (List.mem (String.sub str 0 1) digit_strings)
                        then (Some ((String.sub str 0 1) , suffix str) ) 
                        else None
