open Syntax

let lookup decl x =
    List.mem x decl

let rec check_expr exp declared =
    match exp with
    | NumE _              -> true
    | VarE s              -> lookup declared x
    | OpE(op, exp1, exp2) -> (check_expr exp1 declared) && (check_expr exp2 declared)

let check_stmt stmt declared =
    match stmt with
    | PrintS exp    -> check_expr exp declared
    | AssS(id, exp) -> if (check_expr exp declared)
                       then lookup declared x
                       else false

let check_decl decl declared =
    match decl with
    | SimpDec(id, expr) -> if (check_expr exp declared)
                           then (true, id::declared)

let check_instruction instr declared =
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
