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
    | OpE(op, e1, e2) -> begin match op with
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
                         end
    | AssE(id, exp)   -> match infer_type exp env with
                         | None   -> None
                         | Some t -> match lookup id env with
                                     | Some typ -> if typ = t
                                                   then Some typ
                                                   else None
                                     | None     -> failwith "unbound variable in typechecking - scope checker fucked up"


let check_decl decl env : ((Syntax.decl * env) option) =
    match decl with
    | SimpDec(id, typ, exp) -> if check_type exp env typ
                               then Some (decl, add_env id typ env)
                               else None

let check_stmt stmt env : (Syntax.stmt * env) option =
    match stmt with
    | DeclS decl     -> begin match check_decl decl env with
                        | Some (decl, env) -> Some ((DeclS decl), env)
                        | None             -> None
                        end
    | ExprS expr     -> begin match infer_type expr env with
                        | Some t -> Some (ExprS expr, env)
                        | None   -> None
                        end
    | PrintS(exp, _) -> begin match infer_type exp env with
                        | Some IntT  -> Some (PrintS(exp, Some IntT), env)
                        | Some BoolT -> Some (PrintS(exp, Some BoolT), env)
                        (* not printable *)
                        | _          -> None
                        end

let rec helper prog env =
    match prog with
    | []          -> Some []
    | stmt::prog  -> begin match check_stmt stmt env with
                     | Some (stmt, env) -> 
                         begin match helper prog env with
                         | None      -> None
                         | Some prog -> Some (stmt::prog)
                         end
                     | None -> 
                         None
                     end

let check prog =
    helper prog empty_env
