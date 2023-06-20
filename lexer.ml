type op = Add | Sub | Mul | Div

type token =
    | Number of int
    | Op of op
    | OpenParen
    | ClosedParen

(* lexer interface - lexes to Some (value * rest) *)

let digits = [0;1;2;3;4;5;6;7;8;9]

let digit_strings = List.map string_of_int digits

let suffix str = (String.sub str 1 (String.length str - 1))

let digit_lex str = if (List.mem (String.sub str 0 1) digit_strings)
                        then (Some ((String.sub str 0 1) , suffix str) ) 
                        else None

let rec number_lex_list str acc =
    match (digit_lex str) with
    | None -> ([], str) 
    | Some (a, b) -> number_lex_list b (a::acc)

let number_lex str =
    match (number_lex_list str []) with
    | ([], s) -> None
    | (ds, s) -> Some ((Number (int_of_string (String.concat "" ds))), s)

let operator_lex str =
    match (String.sub str 0 1) with
    | "+" -> Some (Add)
    | "-" -> Some (Sub)
    | "*" -> Some (Mul)
    | "/" -> Some (Div)
    | _ -> None
