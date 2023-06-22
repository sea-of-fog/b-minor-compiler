(* version 0 of the parser, based on the CFG:
 * S -> E
 * E -> (E)
 * E -> E op E
 * op -> + | - | * | /
 *
 * this, however does not work, because:
 * 1) there are left-recursive productions
 * 2) there is not operator precedence
 * 3) it parser expressions to right-associativiy 
 *)

type operator = AddO | SubO | MulO | DivO
type expr =
    | NumE of int
    | OpE of operator * expr * expr

let operator_of_op o =
    match o with
    | Add -> AddO
    | Sub -> SubO
    | Mul -> MulO
    | Div -> DivO

let rec number_parser ts =
        match ts with
        | [] -> None
        | t::rest -> match t with
                     | Number n -> Some (NumE n, rest)
                     | _ -> None

and bracket_parser ts =
    match ts with
    | [] -> None
    | t::rest -> match t with
                 | ClosedParen -> None
                 | Number x -> None
                 | Op o -> None
                 | OpenParen -> match expr_parser rest with
                                | None -> None
                                | Some (exp,rest) -> match rest with
                                                    | ClosedParen::rest -> Some (exp, rest)
                                                    | _ -> None
                                                

and op_parser ts =
        match expr_parser ts with
        | None -> None
        | Some (exp1, rest) -> match rest with
                               | (Op oper)::rest -> match expr_parser rest with
                                                  | None -> None
                                                  | Some (exp2, rest) -> let exp = OpE ((operator_of_op oper), exp1, exp2) in Some (exp, rest)
                               | _ -> None


and expr_parser ts =
    match bracket_parser ts with
    | Some (expr, rest) -> Some (expr, rest)
    | None -> match op_parser ts with
              | Some (expr, rest) -> Some (expr, rest)
              | None -> match number_parser ts with
                        | Some (expr, rest) -> Some (expr, rest)
                        | None -> None

let parse_string str =
    expr_parser (main_lex str)
