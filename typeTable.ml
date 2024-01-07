open Syntax

type env = (location * typ) list

type my_state = { 
    global_env          : env;
    local_env_stack     : env list;
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
    local_env_stack = []}

let return = 
    M.return

let ( let* ) =
    M.( let* )

let run comp = 
    match M.run starting_state comp with
    | M.Ok res -> 
        Ok res
    | M.Fail msg ->
        Fail msg

let fail = M.fail

let add_to_current_scope loc typ =
    let* state = get in
        match state.local_env_stack with
        | [] ->
            set { global_env = (loc,typ)::(state.global_env);
                  local_env_stack = []
                }
        | env::rest ->
            set { global_env = state.global_env;
                  local_env_stack = ((loc, typ)::env)::rest
                }

let open_scope =
    let* state = get in
        set { global_env = state.global_env;
              local_env_stack = []::(state.local_env_stack)}

let close_scope =
    let* state = get in
        match state.local_env_stack with
        | [] ->
            failwith "TypeTable: tried closing global scope"
        | env::rest ->
            set { global_env = state.global_env;
                  local_env_stack = rest }

let rec state_lookup loc global local =
    match local with
    | [] ->
        begin match List.assoc_opt loc global with
            | Some typ 
                -> typ
            | None
                -> failwith "TypeTable: variable not in any scope, scope resolver fucked up"
        end
    | env::rest ->
        match List.assoc_opt loc env with
        | Some typ ->
            typ
        | None ->
            state_lookup loc global rest

let lookup loc =
    let* state = get in
        return @@ state_lookup loc state.global_env state.local_env_stack  
