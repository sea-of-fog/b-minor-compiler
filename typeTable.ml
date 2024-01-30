open Syntax

type 'a env = ('a * typ) list

type my_state = { 
    global_env          : location env;
    local_env_stack     : int env list;
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

let fail = 
    M.fail

let add_to_current_scope loc typ =
    let* state = get in
        match state.local_env_stack with
        | [] ->
            begin match loc with
            | GlobalLoc id ->
            set { state with global_env = ((GlobalLoc id),typ)::(state.global_env)}
            | LocalLoc data ->
                failwith "TypeTable: local variable in global scope, scope resolver fucked up"
            end
        | env::rest ->
            begin match loc with
            | GlobalLoc id ->
                failwith "TypeTable: global variable in local scope, scope resolver fucked up"
            | LocalLoc {pos; scope} ->
                set { state with local_env_stack = ((pos,typ)::env)::rest }
            end

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
            set { state with local_env_stack = rest }

let print_loc = function
    | GlobalLoc id -> id
    | LocalLoc { pos; scope } -> "{ pos = "^(string_of_int pos)^", scope = "^(string_of_int scope)^"}"

let rec state_lookup loc global local =
    match loc with
    | GlobalLoc id ->
        begin match List.assoc_opt loc global with
            | Some typ ->
                typ
            | None ->
                failwith @@ "TypeTable: unbound global variable"^(print_loc loc)^", scope resolver fucked up"
        end
    | LocalLoc { pos; scope } ->
        if scope = 0 
        then match List.assoc_opt pos (List.hd local) with
            | Some typ ->
                typ
            | None ->
                failwith @@ "TypeTable: unbound local variable"^(print_loc loc)^", scope resolver fucked up"
        else state_lookup (LocalLoc { pos = pos; scope = scope - 1 })
                           global 
                          (List.tl local)


let lookup loc =
    let* state = get in
        return @@ state_lookup loc state.global_env state.local_env_stack  
