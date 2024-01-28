open Syntax

type block_state = 
    { temps          : int;
      free_temps     : memory list;
      free_registers : register list;
      name           : string;
      locals         : int;
    }

type my_state = block_state list

(* FIXME: need to do two passes with temp variables -- first generate their numbers and then resolve them *)
(* TODO: separate name and scope stack and write "methods" to access/change them *)

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

let start_state = 
    []

let run comp =  
    M.run start_state comp

let get_top : block_state t =
    let* top::rest = get in
        return top

let set_top new_top =
    let* top::rest = get in
        set @@ new_top::rest

let free = function
    | GlobalMem _ -> 
        return ()
    | LocalMem  _ -> 
        return ()
    | RegisterMem r ->
        let* state = get_top in
        let free = state.free_registers in
        let free = r::free in
            set_top { state with free_registers = free }
    | TempMem ind ->
        let* state = get_top in
        let temps = state.free_temps in
        let temps = (TempMem ind)::temps in
        set_top { state with free_temps = temps }

let alloc = function
    | GlobalLoc s ->
        return @@ GlobalMem s
    | LocalLoc { scope; pos } ->
           return @@ LocalMem (1+pos)
    | TempLoc ->
        let* state = get_top in
        begin match state.free_registers with
        | r::rs ->
                let* () = set_top { state with free_registers = rs }
                in return @@ RegisterMem r
        | [] -> begin match state.free_temps with
                      | t::ts -> let* () = set_top { state with free_temps = ts }
                                 in return t
                      | []    -> let temps = state.temps in
                                 let temps = temps+1 in
                                 let* () = set_top { state with temps = temps }
                                    in return @@ TempMem temps 
                end
        end

let block_data_v2_of_state state =
    { label_v2 = state.name;
      temp_vars_v2 = state.temps;
      local_vars_v2 = state.locals}

let open_scope block_data = 
    let* top::rest = get in
    let new_top = { temps = 0;
                    locals = block_data.local_vars;
                    free_temps = [];
                    free_registers = R10::R11::R12::R13::R14::R15::[];
                    name = block_data.label} in
    set @@ new_top::top::rest

let close_scope =
    let* top::rest = get in
    let* () = set rest in
    return @@ block_data_v2_of_state top
