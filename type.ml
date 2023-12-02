open Syntax

let lookup (id : string) (env : (string * Syntax.typ) list) : Syntax.typ option =
    List.assoc_opt id env

let empty_env = []

let add_env id typ env =
    (id, typ)::env

let rec check_type exp env t =
    let Some inferred_type = infer_type exp env in
        if t = inferred_type
        then true
        else false

and infer_type exp env : Syntax.typ option =
    match exp with
    | NumE _          -> Some IntT
    | TrueE           -> Some BoolT
    | FalseE          -> Some BoolT
    | VarE x          -> lookup x env
    | OpE(op, e1, e2) -> match op with
                         | Add | Sub | Mul | Div -> if check_type e1 env IntT
                                                    then if check_type e2 env IntT
                                                         then Some IntT
                                                         else None
                                                    else None
                         | And | Or              -> if check_type e1 env BoolT
                                                    then if check_type e2 env BoolT
                                                         then Some BoolT 
                                                         else None
                                                    else None


let check_decl decl env =
    match decl with
    | SimpDec(id, typ, exp) -> if check_type exp env typ
                               then (true, add_env id typ env)
                               else (false, add_env id typ env)

let check_stmt stmt env =
    match stmt with
    | AssS(id, exp) -> failwith "not implemented"
    | PrintS exp    -> failwith "not implemented"
                    

let check_instr instr env =
    match instr with
    | Expr expr -> begin match infer_type expr env with
                   | Some t -> (true, env)
                   | None   -> (false, env)
                   end
    | Stmt stmt -> (check_stmt stmt env, env)
    | Decl decl -> check_decl decl env

let rec helper prog env =
    match prog with
    | []          -> true
    | instr::prog -> match check_instr instr env with
                     | (true,  env)  -> helper prog env
                     | (false, env)  -> false

let check prog =
    helper prog empty_env
