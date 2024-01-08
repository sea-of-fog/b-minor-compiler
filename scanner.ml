open Syntax

(*
    TODO:
        1. add error handling
        3. structure this better: use sequencing/alternative of parsers
 *)

(* lexer interface - lexes to Some (value * rest) *)

let digits = [0;1;2;3;4;5;6;7;8;9]

let digit_strings = List.map string_of_int digits

(* conjuction of predicates *)
let (&&&) p1 p2 = 
    fun x -> (p1 x) && (p2 x)

let lowercase_letter c =
    (97 <= (Char.code c)) && ((Char.code c) <= 122)

let suffix_from from str = 
    (String.sub str from (String.length str - from))

let suffix str = 
    suffix_from 1 str

let letter_lex str = 
    if (String.length str = 0)
    then None
    else if lowercase_letter str.[0]
        then (Some ((String.sub str 0 1), suffix str)) 
        else None

let rec id_lex_list str acc =
    match (letter_lex str) with
    | None -> (acc, str) 
    | Some (a, b) -> id_lex_list b (a::acc)

let id_lex str =
    match (id_lex_list str []) with
    | ([], s) -> 
        None
    | (ls, s) -> 
        begin match String.concat "" (List.rev ls) with
        | "let"    -> Some (Keyword Let, s)
        | "print"  -> Some (Keyword Print, s)
        | "true"   -> Some (Keyword True, s)
        | "false"  -> Some (Keyword False, s)
        | "int"    -> Some (Keyword Int, s)
        | "bool"   -> Some (Keyword Bool, s)
        | id       -> Some (Id id, s)
        end

let digit_lex str = 
    if (String.length str = 0)
    then None
    else if (List.mem (String.sub str 0 1) digit_strings)
        then (Some ((String.sub str 0 1) , suffix str) ) 
        else None

let rec number_lex_list str acc =
    match (digit_lex str) with
    | None -> (acc, str) 
    | Some (a, b) -> number_lex_list b (a::acc)

let number_lex str =
    match (number_lex_list str []) with
    | ([], s) -> 
        None
    | (ds, s) -> 
        Some ((Number (int_of_string (String.concat "" (List.rev ds)))), s)

let operator_lex str =
    if (String.length str = 0)
    then None
    else match (String.sub str 0 1) with
         | "+" -> Some ((Op (Add)), suffix str)
         | "-" -> Some ((Op (Sub)), suffix str)
         | "*" -> Some ((Op (Mul)), suffix str)
         | "/" -> Some ((Op (Div)), suffix str)
         | _   -> if (String.length str < 2)
                  then None
                  else begin match (String.sub str 0 2) with
                       | "&&" -> Some ((Op And), suffix_from 2 str)
                       | "||" -> Some ((Op Or), suffix_from 2 str)
                       | _    -> None
                       end

let paren_lex str =
    if (String.length str = 0)
    then None
    else match (String.sub str 0 1) with
        | "(" -> Some ((OpenParen), suffix str)
        | ")" -> Some ((ClosedParen), suffix str)
        | "{" -> Some ((OpenCurly), suffix str)
        | "}" -> Some ((ClosedCurly), suffix str)
        | ";" -> Some ((SemiColon), suffix str)
        | ":" -> Some ((Colon), suffix str)
        | "=" -> Some ((Equal), suffix str)
        | _ -> None

let rec lex_it str acc =
    if str = "" then acc
    else if List.mem (str.[0]) [' '; '\t'; '\n'; '\r']
    then lex_it (suffix str) acc
    else      match id_lex str with
              | Some (token, rest) -> lex_it rest (token::acc)
              | None -> match paren_lex str with
                        | Some (token, rest) -> lex_it rest (token::acc) 
                        | None -> match operator_lex str with
                                  | Some (token, rest) -> lex_it rest (token::acc)
                                  | None -> match number_lex str with
                                            | Some (token, rest) -> lex_it rest (token::acc)
                                            | None -> acc

let main_lex str =
    List.rev (lex_it str [])
