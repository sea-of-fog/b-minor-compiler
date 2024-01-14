type my_state = 
    { scope_stack = int list;
      temp_stack  = int list;
      temps       = int;
      busy_registers = register list;
      free_register  = register list
    }

let rec compute_offset scope_stack scope pos =
    match scope_stack with
    | size::rest ->
        if scope = 0
        (* look out for this +- 1 *)
        then size - pos + 1
        else size + compute_offset rest (scope - 1) pos

