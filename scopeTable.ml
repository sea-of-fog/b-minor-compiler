open Syntax

type env = (string * int) list

type my_state = { 
    global_env          : string list;
    local_env_stack     : env list;
    current_scope_size  : int;
    label_num           : int;
}

module State = struct
    type t = my_state
end

module M = ErrorStateMonad.Make(State)
open M

type 'a t = 
    'a M.t

type 'a res =
    | Ok   of 'a
    | Fail of string

let starting_state = 
   {global_env = [];
    local_env_stack = [];
    current_scope_size = 0;
    label_num = 0 }

let return = M.return
let ( let* ) = M.( let* )
let run comp = 
    match M.run starting_state comp with
    | M.Ok res -> 
        Ok res
    | M.Fail msg ->
        Fail msg

let fail = M.fail

let add_to_current_scope id =
    let* state = get in
        match state.local_env_stack with
        | [] ->
            begin match List.mem id state.global_env with
            | false -> 
                let* () = set { global_env = id::state.global_env;
                                local_env_stack = state.local_env_stack;
                                current_scope_size = 1 + state.current_scope_size;
                                label_num = state.label_num
                              } in
                    return @@ GlobalLoc id
            | true ->
                fail @@ "variable "^id^" is already bound in current (global) scope"
            end
        | env::rest ->
            begin match List.assoc_opt id env with
            | None -> 
                let pos = state.current_scope_size in
                    let* () = set { global_env = state.global_env;
                                    local_env_stack = ((id, pos)::env)::rest;
                                    current_scope_size = pos + 1;
                                    label_num = state.label_num
                                  } in
                        return @@ LocalLoc { scope = 0; pos = pos}
            | Some _ ->
                fail @@ "variable "^id^" is already bound in current (local) scope"
            end

let open_scope =
    let* state = get in
        set { local_env_stack = []::(state.local_env_stack);
              global_env = state.global_env;
              current_scope_size = 0;
              label_num = state.label_num}     

let close_scope = 
    let* state = get in
        match state.local_env_stack with
        | _::rest ->
            set { local_env_stack = rest;
                  global_env = state.global_env;
                  current_scope_size = 0;
                  label_num = state.label_num}     
        | [] ->
            failwith "ScopeTable: Tried closing global scope"

let generate_label =
    let* state = get in
        let num = state.label_num in
            let* () = set { local_env_stack = state.local_env_stack;
                            global_env = state.global_env;
                            current_scope_size = state.current_scope_size;
                            label_num = num + 1
                          } in 
                return @@ "B"^(string_of_int num)

let current_scope_size =
    let* state = get in
        return state.current_scope_size

let rec assoc_ind id env =
    match env with
    | [] ->
        None
    | (key, value)::env when id = key ->
        Some 1
    | _::env ->
        match assoc_ind id env with
        | None -> 
            None
        | Some ind ->
            Some (ind + 1)

let rec local_lookup id (stack : env list) : location option = 
    match stack with
    | [] ->
        None
    | env::rest ->
        begin match List.assoc_opt id env with
        | Some ind ->
          Some (LocalLoc { pos = ind;
            scope = 0
          })
        | None ->
            begin match local_lookup id rest with
            | None ->
                None
            | Some (LocalLoc { pos; scope }) ->
                Some (LocalLoc { pos; scope = scope + 1 })
            end
        end

let lookup id = 
    let* state = get in
        match local_lookup id state.local_env_stack with
        | None ->
            begin match List.mem id state.global_env with
            | false ->
                fail @@ "unbound variable "^id 
            | true ->
                return @@ GlobalLoc id
            end
        | Some location ->
            return location
