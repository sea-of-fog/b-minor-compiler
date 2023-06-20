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
    | (ds, s) -> Some ((Number (int_of_string (String.concat "" (List.rev ds)))), s)

let operator_lex str =
    match (String.sub str 0 1) with
    | "+" -> Some ((Op (Add)), suffix str)
    | "-" -> Some ((Op (Sub)), suffix str)
    | "*" -> Some ((Op (Mul)), suffix str)
    | "/" -> Some ((Op (Div)), suffix str)
    | _ -> None

let paren_lex str =
    match (String.sub str 0 1) with
    | "(" -> Some ((OpenParen), suffix str)
    | ")" -> Some ((ClosedParen), suffix str)
    | _ -> None

let rec lex_it str acc =
    match paren_lex str with
    | Some (token, rest) -> lex_it rest (token::acc) 
    | None -> match operator_lex str with
              | Some (token, rest) -> lex_it rest (token::acc)
              | None -> match number_lex str with
                        | Some (token, rest) -> lex_it rest (token::acc)
                        | None -> acc

let main_lex str =
    List.rev (lex_it str [])
