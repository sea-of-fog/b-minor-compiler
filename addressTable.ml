type block_state = 
    { scope_stack    = int list;
      temp_stack     = int list;
      temps          = int;
      free_temps     = int list;
      busy_registers = register list;
      free_register  = register list
    }

type my_state = block_state list

module State = struct
    type t = my_state
end

module M = StateMonad.Make(State)
open M

type t = M.t

let return = 
    failwith "not implemented"

let ( let* ) =
    M.( let* )

let global_state = 
    failwith "not implemented"

let run =  
    M.run global_state

let get_top =
    failwith "not implemented"

let set_top =
    failwith "not implemented"

let rec compute_offset scope_stack scope pos =
    match scope_stack with
    | size::rest ->
        if scope = 0
        (* look out for this +- 1 *)
        then size - pos + 1
        else size + compute_offset rest (scope - 1) pos


let free = function
    | GlobalMem _ -> 
        return ()
    | LocalMem  _ -> 
        return ()
    | RegisterMem r ->
        let* state = get_top in
        let (busy, free) = (state.busy_registers, state.free_registers) in
        let busy = List.filter (<> r) busy in
        let free = r::free in
        let* () = set_top {  }
    | TempMem ind ->
        let* state = get_top in
        let temp_list = state.temp_list in
        let temp_list = ind::temp_list in
        let* () = set {  }

let alloc = function
    | GlobalLoc s ->
        return @@ GlobalMem s
    | LocalLoc { scope, pos } ->
    | TempLoc ->
        let* state = get_top in
        begin match state.free_registers with
        | r::rs ->
            let* () = set_top {} in
                return r
        | [] -> begin match state.free_temps with
                      | t::ts -> let* () = set_top {} in
                                    return t
                      | []    -> let temps = state.temps in
                                 let temps = temps+1
                                    let* () = set_top { temps } in
                                        return @@ temps 
        end

let state_of_block_data =
    failwith "not implemented"

let block_data_of_state =
    failwith "not implemented"

let open_scope block_data = 
    let* state = get in
        set @@ (state_of_block_data block_data)

let close_scope =
    let* top::rest = get in
    let* () = set rest in
    return @@ block_data_of_state top
