open ScopeTable
open Syntax

(* This module checks if every variable used has already been declared *)

let rec resolve_expr expr = 
    match expr with
    | NumE n -> 
        return @@ NumAE (TempLoc, n) 
    | OpE (op, e1, e2) ->
        let* ann_e1 = resolve_expr e1 in
            let* ann_e2 = resolve_expr e2 in
                return @@ OpAE (TempLoc, op, ann_e1, ann_e2)
    | VarE id -> 
        let* location = lookup id in
            return @@ VarAE location
    | AssE (id, e) ->
        let* location = lookup id in
            let* ann_e = resolve_expr e in
                return @@ AssAE (location, ann_e)
    | TrueE -> 
        return @@ TrueAE TempLoc
    | FalseE ->
        return @@ FalseAE TempLoc

let resolve_decl decl =
    match decl with
    | SimpDec (id, typ, e) -> 
        let* ann_e = resolve_expr e in
            let* location = add_to_current_scope id in
                return @@ SimpADec (location, typ, ann_e)

let rec resolve_stmt stmt =
    match stmt with
    | DeclS decl ->
        let* ann_decl = resolve_decl decl in
            return @@ DeclAS ann_decl
    | ExprS expr  -> 
        let* ann_expr = resolve_expr expr in
            return @@ ExprAS ann_expr
    | PrintS expr -> 
        let* ann_expr = resolve_expr expr in
            return @@ PrintAS ann_expr
    | BlockS ss -> 
        let* label = generate_label in
            let* () = open_scope in
                let* ann_ss = resolve_prog ss in
                    let* block_cnt = current_scope_size in
                        let* () = close_scope in
                            return @@ BlockAS ({ label = label; local_vars = block_cnt }, ann_ss)

and resolve_prog prog =
    match prog with
    | []         -> return []
    | stmt::prog -> 
        let* ann_stmt = resolve_stmt stmt in
            let* ann_rest = resolve_prog prog in
                return @@ ann_stmt::ann_rest

let resolve prog =
    run @@ resolve_prog prog
