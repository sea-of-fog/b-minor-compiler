open Syntax

type block_state = 
    { scope_stack    : int list;
      temps          : int;
      free_temps     : int list;
      free_registers : register list;
      name           : string;
    }

type my_state = block_state list

(* TODO: separate name and scope stack and write "methods" to access/change them *)
(* TODO: add getters/setters *)

module State = struct
    type t = my_state
end

module M = StateMonad.Make(State)

type 'a t = 'a M.t

let return = 
    M.return

let ( let* ) =
    M.( let* )

let get =
    M.get

let set =
    M.set

let global_state = 
    { scope_stack = [];
      temps = 0;
      free_temps = [];
      free_registers = R10::R11::R12::R13::R14::R15::[];
      name = "global"
    }::[]

let run =  
    M.run global_state

let get_top : block_state t =
    let* top::rest = get in
        return top

let set_top new_top =
    let* top::rest = get in
        set @@ new_top::rest

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
        let free = state.free_registers in
        let free = r::free in
        set_top { name = state.name;
                  scope_stack = state.scope_stack;
                  free_temps = state.free_temps;
                  temps = state.temps;
                  free_registers = free}
    | TempMem ind ->
        let* state = get_top in
        let temps = state.free_temps in
        let temps = ind::temps in
        set_top { name = state.name;
                  scope_stack = state.scope_stack;
                  free_temps = temps;
                  temps = state.temps;
                  free_registers = state.free_registers}

let alloc = function
    | GlobalLoc s ->
        return @@ GlobalMem s
    | LocalLoc { scope; pos } ->
        let* state = get_top in
           return @@ LocalMem (compute_offset state.scope_stack scope pos) 
    | TempLoc ->
        let* state = get_top in
        begin match state.free_registers with
        | r::rs ->
                let* () = set_top { name = state.name;
                                    free_registers = rs; 
                                    temps = state.temps; 
                                    free_temps = state.free_temps; 
                                    scope_stack = state.scope_stack} 
                in return r
        | [] -> begin match state.free_temps with
                      | t::ts -> let* () = set_top { name = state.name;
                                                     free_registers = state.free_registers;
                                                     temps = state.temps;
                                                     free_temps = ts;
                                                     scope_stack = state.scope_stack} 
                                 in return @@ TempMem 
                      | []    -> let temps = state.temps in
                                 let temps = temps+1 in
                                    let* () = set_top { temps = temps;
                                                        name = state.name;
                                                        free_registers = state.free_registers;
                                                        temps = state.temps;
                                                        free_temps = ts;
                                                        scope_stack = state.scope_stack}
                                    in return @@ TempMem temps 
                end
        end

let state_of_block_data =
    failwith "not implemented"

let block_data_of_state =
    failwith "not implemented"

let add_top s =
    let* state = get in
        set @@ s::state

let open_scope block_data = 
    add_top (state_of_block_data block_data)

let close_scope =
    let* top::rest = get in
    let* () = set rest in
    return @@ block_data_of_state top
