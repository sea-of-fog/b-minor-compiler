open Syntax

let lookup decl x =
    List.mem x decl

let rec check_expr exp declared =
    match exp with
    | NumE _              -> true
    | TrueE               -> true
    | FalseE              -> true
    | VarE s              -> lookup declared s
    | OpE(op, exp1, exp2) -> (check_expr exp1 declared) && (check_expr exp2 declared)

let check_stmt stmt declared =
    match stmt with
    | PrintS exp    -> check_expr exp declared
    | AssS(id, exp) -> if (check_expr exp declared)
                       then lookup declared id
                       else false

let check_decl decl declared =
    match decl with
    | SimpDec(id, typ, exp) -> if (check_expr exp declared)
                               then (true, id::declared)
                               else (false, id::declared)

let check_instr instr declared =
    match instr with
    | Expr exp  -> (check_expr exp declared, declared)
    | Stmt stmt -> (check_stmt stmt declared, declared)
    | Decl decl -> check_decl decl declared

let rec check_prog_helper prog declared =
    match prog with
    | [] -> true
    | instr::prog -> let (res, declared) = (check_instr instr declared) 
                     in if res
                        then check_prog_helper prog declared
                        else false


let check_if_declared prog =
    check_prog_helper prog []
