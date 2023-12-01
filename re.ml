type re =
    | Eps
    | Const  of char
    | Alter  of re * re
    | Concat of re * re
    | Kleene of re * re

type lexer = string -> (string * string) option

let const_re_of_string str =
    if str = ""
    then Eps
    else Concat(Const s.[0], const_re_of_string )

let alter_of_list res =
    failwith "not implemented"

let rec lexer_of_re re str =
    match re with
    | Eps              -> Some ("", str)
    | Const c          -> if s.[0] = c
                          then Some(String.make 1 c, )
                          else None
    | Alter(re1, re2)  -> begin match lexer_of_re re1 str with
                          | Some x -> Some x
                          | None   -> lexer_of_re re2 str
    | Concat(re1, re2) -> begin match lexer_of_re re1 str with
                          | None         -> None
                          | Some(m, rst) -> begin match lexer_of_re re2 rst with
                                            | None         -> None
                                            | Some(n, rst) -> Some(m^n, rst)
    | Kleene re        -> begin match lexer_of_re re str with
                          | None          -> ("", str)
                          | Some (m, rst) -> let Some (n, rst) = lexer_of_re (Kleene re) rst in
                                                Some (m^n, rst)
