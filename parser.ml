type op = Add | Sub | Mul | Div
type expr =
    | NumE of int
    | OpE of op * expr * expr

let number_parser ts =
    match ts with
    | [] -> None
    | t::rest -> match t with
                 | Number n -> Some (NumE n, rest)
                 | _ -> None

let bracket_parser ts =
    match ts with
    | [] -> None
    | t::rest -> match t with
                 | OpenBracket -> match expr_parser rest with
                                  | None -> None
                                  | Some exp::rest -> match rest with
                                                      | ClosedBracket::rest -> Some (exp, rest)
                                                      | _ -> None

let op_parser ts =
    match expr_parser ts with
    | None -> None
    | Some (exp1, rest) -> match rest with
                           | (Op op)::rest -> match expr_parser rest with
                                              | None -> None
                                              | Some (exp2, rest) -> OpE op exp1 exp2
                           | _ -> None


let rec expr_parser ts =
    match bracket_parser ts with
    | Some (expr, rest) -> Some (expr, rest)
    | None -> match op_parser ts with
              | Some (expr, rest) -> (expr, rest)
              | None -> match number_parser with
                        | Some (expr, rest) -> (expr, rest)
                        | None
