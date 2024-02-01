open Syntax
open TypeTable

let extract_type exp =
    match exp with
    | NumAE _                 -> IntT
    | TrueAE _                -> BoolT
    | FalseAE _               -> BoolT
    | VarAE (_, typ)          -> typ
    | OpAE((_, typ), _, _, _) -> typ 
    | AssAE((_, typ), _)      -> typ 

let string_of_type typ =
    match typ with
    | IntT  -> "int"
    | BoolT -> "bool"

let rec check_expr expected_type exp =
    let* ann_exp = infer_expr exp in
        let actual_type = extract_type ann_exp in
            if actual_type = expected_type
            then return ann_exp
            else fail @@ "expected expression of type "^(string_of_type expected_type)^
                ", but expression has type "^(string_of_type actual_type)

and infer_expr exp =
    match exp with
    | NumAE(loc, n) ->
        return @@ NumAE ((loc, IntT), n)
    | TrueAE loc -> 
        return @@ TrueAE (loc, BoolT)
    | FalseAE loc -> 
        return @@ FalseAE (loc, BoolT)
    | VarAE loc -> 
        let* typ = lookup loc in
            return @@ VarAE (loc, typ)
    | OpAE(loc, op, e1, e2) -> 
        begin match op with
        | Add | Sub | Mul | Div ->
            let* ann_e1 = check_expr IntT e1 in
                let* ann_e2 = check_expr IntT e2 in
                    return @@ OpAE((loc, IntT), op, ann_e1, ann_e2)
        | Or | And ->
            let* ann_e1 = check_expr BoolT e1 in
                let* ann_e2 = check_expr BoolT e2 in
                    return @@ OpAE((loc, BoolT), op, ann_e1, ann_e2)
        | Lt | Gt | Leq | Geq | Eq | Neq ->
            let* ann_e1 = infer_expr e1 in
                let typ = extract_type ann_e1 in
                    let* ann_e2 = check_expr typ e2 in
                        return @@ OpAE((loc, BoolT), op, ann_e1, ann_e2)
        end
    | AssAE(loc, exp) -> 
        let* expected_type = lookup loc in
            let* ann_exp = check_expr expected_type exp in
                return @@ AssAE((loc, expected_type), ann_exp)

let infer_decl decl =
    match decl with
    | SimpADec(loc, typ, exp) -> 
        let* ann_exp = check_expr typ exp in
            return @@ SimpADec((loc, typ), typ, ann_exp)

let rec infer_stmt stmt =
    match stmt with
    | DeclAS decl ->
        let* ann_decl = infer_decl decl in
            begin match ann_decl with
            | SimpADec((loc, typ), _, _) ->
                let* () = add_to_current_scope loc typ in
                    return @@ DeclAS ann_decl
            end
    | ExprAS exp ->
        let* ann_exp = infer_expr exp in
            return @@ ExprAS ann_exp
    | PrintAS exp -> 
        let* ann_exp = infer_expr exp in
            return @@ PrintAS ann_exp
    | BlockAS(block_data, ss) ->
        let* () = open_scope in
            let* ann_ss = infer_block ss in
                let* () = close_scope in
                    return @@ BlockAS(block_data, ann_ss)
    | IfAS(exp, stmt) ->
        let* ann_exp = check_expr BoolT exp in
            let* ann_stmt = infer_stmt stmt in
                return @@ IfAS(ann_exp, ann_stmt)
    | WhileAS(exp, stmt) ->
        let* ann_exp = check_expr BoolT exp in
            let* ann_stmt = infer_stmt stmt in
                return @@ WhileAS(ann_exp, ann_stmt)

and infer_block ss =
    match ss with
    | [] ->
        return []
    | stmt::ss ->
        let* ann_stmt = infer_stmt stmt in
            let* ann_ss = infer_block ss in
                return @@ ann_stmt::ann_ss
                
let check scoped_prog =
    run @@ infer_block scoped_prog
