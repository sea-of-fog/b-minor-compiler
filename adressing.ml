type register =
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15

type my_state = 
    { scope_stack = int list;
      busy_registers = register list;
      free_register  = register list}

let rec compute_offset scope_stack scope pos =
    match scope_stack with
    | size::rest ->
        if scope = 0
        (* look out for this +- 1 *)
        then size - pos + 1
        else size + compute_offset rest (scope - 1) pos

let generate_expression =
    failwith "not implemented"

let generate_decl =
    failwith "not implemented"

let generate_stmt =
    failwith "not implemented"

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
