open Syntax
open AddressTable

let extract_data exp =
    match exp with
    | NumAE(data, _)     -> data
    | OpAE(data,_, _, _) -> data
    | VarAE(data)        -> data
    | TrueAE(data)       -> data
    | FalseAE(data)      -> data
    | AssAE(data, _)     -> data

let extract_location exp =
    fst @@ extract_data exp

let rec alloc_expr = function
    | NumAE((loc, typ), n) ->
        let* address = alloc loc in
            return @@ NumAE((address, typ), n)
    | VarAE((loc, typ)) ->
        let* address = alloc loc in
            return @@ VarAE((address, typ))
    | OpAE((loc, typ), op, e1, e2) ->
        let* all_e1 = alloc_expr e1 in
        let* all_e2 = alloc_expr e2 in
        let (loc1, loc2) = (extract_location all_e1, extract_location all_e2) in
        let* () = free loc2 in
        let* () = free loc1 in
        let* new_loc = alloc loc in
            return @@ OpAE((new_loc, typ), op, all_e1, all_e2)
    | AssAE((loc, typ), e) ->
        let* all_e = alloc_expr e in
            let* new_loc = alloc loc in
                return @@ AssAE((new_loc, typ), all_e)
    | TrueAE((loc, typ)) ->
        let* new_loc = alloc loc in
            return @@ TrueAE((new_loc, typ))
    | FalseAE((loc, typ)) ->
        let* new_loc = alloc loc in
            return @@ FalseAE((new_loc, typ))

let alloc_decl decl =
    match decl with
    | SimpADec((loc, typ1), typ2, exp) ->
        let* all_exp = alloc_expr exp in
        let* loc = alloc loc in
        let* () = free @@ extract_location all_exp in
            return @@ SimpADec((loc, typ1), typ2, all_exp)

let rec alloc_stmt stmt =
    match stmt with
    | ExprAS e -> 
        let* all_e = alloc_expr e in
        let* () = free @@ extract_location all_e in
            return @@ ExprAS all_e
    | DeclAS d -> 
        let* all_decl = alloc_decl d in
            return @@ DeclAS all_decl
    | PrintAS e -> 
        let* all_e = alloc_expr e in
        let* () = free @@ extract_location all_e in
            return @@ PrintAS all_e         
    | BlockAS(b, ss) -> 
        let* () = open_scope b in
            let* alloc_ss = alloc_prog ss in
                let* new_block_data = close_scope in
                    return @@ BlockAS(new_block_data, alloc_ss)
    | IfAS(exp, stmt) ->
        let* all_exp = alloc_expr exp in
            let* all_stmt = alloc_stmt stmt in
                return @@ IfAS(all_exp, all_stmt)

and alloc_prog prog = 
    match prog with
    | [] ->
        return []
    | stmt::prog ->
        let* stmt = alloc_stmt stmt in
            let* prog = alloc_prog prog in
                return @@ stmt::prog

let make_global_a_block prog =
    [BlockAS ({ label = "GLOBAL";
                local_vars = 0;}, prog)]

let generate_addresses prog =
    run @@ alloc_prog @@ make_global_a_block prog
