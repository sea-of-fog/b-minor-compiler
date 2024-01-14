(* type my_state = *) 
(*     { scope_stack = int list; *)
(*       temp_stack  = int list; *)
(*       temps       = int; *)
(*       busy_registers = register list; *)
(*       free_register  = register list *)
(*     } *)

(* let rec compute_offset scope_stack scope pos = *)
(*     match scope_stack with *)
(*     | size::rest -> *)
(*         if scope = 0 *)
(*         (1* look out for this +- 1 *1) *)
(*         then size - pos + 1 *)
(*         else size + compute_offset rest (scope - 1) pos *)

open AddressTable

let extract_location exp =
    match exp with
    | NumAE(loc, _)     -> loc
    | OpAE(loc, _, _)   -> loc
    | VarAE(loc)        -> loc
    | TrueE(loc)        -> loc
    | FalseE(loc)       -> loc
    | AssE(loc)         -> loc

let alloc_expr exp =
    match exp with
    | NumAE((loc, typ), n) ->
        let* new_loc = alloc loc in
            return @@ NumAE((new_loc, typ), n)
    | VarAE((loc, typ)) ->
        let* new_loc = alloc loc in
            return @@ VarAE((new_loc, typ))
    | OpAE((loc, typ), e1, e2) ->
        let* all_e1 = alloc_expr e2 in
            let* all_e2 = alloc_expr e2 in
                let (loc1, loc2) = (extract_location all_e1, extract_location all_e2) in
                    let* () = free loc2 in
                        return @@ OpAE((loc1, typ), all_e1, all_e2)
    | AssAE(loc, e) ->
        let* all_e = alloc_expr e in
            let* new_loc = alloc loc in
                return @@ AssAE(alloc loc, )
    | TrueAE(loc) ->
        let* new_loc = alloc loc in
            return @@ TrueAE(new_loc)
    | FalseAE(loc) ->
        let* new_loc = alloc loc in
            return @@ FalseAE(new_loc)

let alloc_decl decl =
    match decl with
    | SimpADec((loc, typ1), typ2, exp) ->
        let* ann_exp = alloc_expr exp in
            let* loc = alloc loc in
                return @@ SimpADec((loc, typ1), typ2, exp)


let alloc_stmt stmt =
    match stmt with
    | ExprAS e -> 
        alloc_expr e
    | DeclAS d -> 
        alloc_decl d
    | PrintAS e -> 
        let* all_e = alloc_expr e in
            return @@ PrintAS all_e         
    | BlockSS(b, ss) ->

let generate_prog prog = 
    match prog with
    | [] ->
        return []
    | stmt::prog ->
        let* stmt = generate_stmt stmt in
            let* prog = generate_prog prog in
                return @@ stmt::prog

let generate_adresses prog =
    run @@ generate_prog prog
