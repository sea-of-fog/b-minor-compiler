open Syntax

type env = (string * Syntax.typ) list

let lookup (id : string) (env : env) : Syntax.typ option =
    List.assoc_opt id env

let empty_env = []

let add_env id typ env =
    (id, typ)::env

let rec check_type exp env t =
    match infer_type exp env with
    | None ->
        false
    | Some inferred_type ->
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


let check_decl decl env : ((Syntax.decl * env) option) =
    match decl with
    | SimpDec(id, typ, exp) -> if check_type exp env typ
                               then Some (decl, add_env id typ env)
                               else None

let check_stmt stmt env : (Syntax.stmt option) =
    match stmt with
    | AssS(id, exp) -> begin match infer_type exp env with
                       | None          -> None
                       | Some exp_type -> begin match lookup id env with
                                          | None         -> None
                                          | Some id_type -> if exp_type = id_type
                                                            then Some stmt
                                                            else None
                                          end
                       end
    | PrintS(exp, _) -> begin match infer_type exp env with
                        | Some IntT  -> Some (PrintS(exp, Some IntT))
                        | Some BoolT -> Some (PrintS(exp, Some BoolT))
                        (* not printable *)
                        | _          -> None
                        end
                    

let check_instr instr env : (Syntax.instr * env) option =
    match instr with
    | Expr expr -> begin match infer_type expr env with
                   | Some t -> Some (Expr expr, env)
                   | None   -> None
                   end
    | Stmt stmt -> begin match check_stmt stmt env with
                   | Some stmt -> Some (Stmt stmt, env)
                   | None      -> None
                   end
    | Decl decl -> begin match check_decl decl env with
                   | Some (decl, env) -> Some ((Decl decl), env)
                   | None             -> None
                   end

let rec helper prog env =
    match prog with
    | []          -> Some []
    | instr::prog -> begin match check_instr instr env with
                     | Some (instr, env) -> 
                         begin match helper prog env with
                         | None      -> None
                         | Some prog -> Some (instr::prog)
                         end
                     | None -> 
                         None
                     end

let check prog =
    helper prog empty_env
