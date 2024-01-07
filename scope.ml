open Syntax

(* This module checks if every variable used has already been declared *)

let empty_env =
    []

let lookup decl x =
    List.mem x decl

let rec check_expr exp declared =
    match exp with
    | NumE _              -> true
    | TrueE               -> true
    | FalseE              -> true
    | VarE s              -> lookup declared s
    | OpE(op, exp1, exp2) -> (check_expr exp1 declared) && (check_expr exp2 declared)
    | AssE(name, exp)     -> (lookup declared name) && (check_expr exp declared)

let check_decl decl declared =
    match decl with
    | SimpDec(id, typ, exp) -> if (check_expr exp declared)
                               then (true, id::declared)
                               else (false, id::declared)

let check_stmt stmt declared =
    match stmt with
    | DeclS decl     -> check_decl decl declared
    | ExprS exp      -> (check_expr exp declared, declared)
    | PrintS(exp, _) -> (check_expr exp declared, declared)

let rec check_prog_helper prog declared =
    match prog with
    | [] -> true
    | stmt::prog -> let (res, declared) = (check_stmt stmt declared) in
                        if res
                        then check_prog_helper prog declared
                        else false

let check_if_declared prog =
    check_prog_helper prog empty_env 
