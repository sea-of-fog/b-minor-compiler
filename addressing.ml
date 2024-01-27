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

let rec alloc_expr exp =
    match exp with
    | NumAE((loc, typ), n) ->
        let* address = alloc loc in
            return @@ NumAE((address, typ), n)
    | VarAE((loc, typ)) ->
        let* address = alloc loc in
            return @@ VarAE((address, typ))
    | OpAE((loc, typ), op, e1, e2) ->
        let* all_e1 = alloc_expr e2 in
            let* all_e2 = alloc_expr e2 in
                let (loc1, loc2) = (extract_location all_e1, extract_location all_e2) in
                    let* () = free loc2 in
                        return @@ OpAE((loc1, typ), op, all_e1, all_e2)
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
                return @@ SimpADec((loc, typ1), typ2, all_exp)

let alloc_stmt stmt =
    match stmt with
    | ExprAS e -> 
        alloc_expr e
    | DeclAS d -> 
        let* all_decl = alloc_decl d in
            return @@ DeclAS all_decl
    | PrintAS e -> 
        let* all_e = alloc_expr e in
            return @@ PrintAS all_e         
    | BlockAS(b, ss) -> 
        let* () = open_scope b in
            let* alloc_ss = generate_prog ss in
                let* new_block_data = close_scope in
                    return @@ BlockSS(new_block_data, alloc_ss)

let rec generate_prog prog = 
    match prog with
    | [] ->
        return []
    | stmt::prog ->
        let* stmt = generate_stmt stmt in
            let* prog = generate_prog prog in
                return @@ stmt::prog

let generate_adresses prog =
    run @@ generate_prog prog
