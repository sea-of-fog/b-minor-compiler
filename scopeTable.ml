open Syntax

type env = (string * int) list

type block_state = { 
    global_env          : string list;
    local_env           : env;
    current_scope_size  : int;
    label_num           : int;
}

type my_state =
    block_state list

(* TODO: make the label number a separate part of state *)
(* TODO: write accessor functions for state components *)
(* TODO: return to old state, but pass label_num carefully *)

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
    [{global_env = [];
    local_env = [];
    current_scope_size = 0;
    label_num = 0 }]

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
        match state with
        | glob::[] ->
            begin match List.mem id glob.global_env with
            | false -> 
                let* () = set ({ global_env = id::glob.global_env;
                                local_env = glob.local_env;
                                current_scope_size = 1 + glob.current_scope_size;
                                label_num = glob.label_num
                                }::[]) in
                    return @@ GlobalLoc id
            | true ->
                fail @@ "variable "^id^" is already bound in current (global) scope"
            end
        | loc::rest ->
            begin match List.assoc_opt id loc.local_env with
            | None -> 
                let pos = loc.current_scope_size in
                    let* () = set ({ global_env = loc.global_env;
                                    local_env = ((id, pos)::loc.local_env);
                                    current_scope_size = pos + 1;
                                    label_num = loc.label_num
                                  }::rest) in
                        return @@ LocalLoc { scope = 0; pos = pos}
            | Some _ ->
                fail @@ "variable "^id^" is already bound in current (local) scope"
            end

let open_scope =
    let* top::rest = get in
    set ({    local_env = [];
              global_env = [];
              current_scope_size = 0;
              label_num = top.label_num}::top::rest)

let close_scope = 
    let* state = get in
        match state with
        | [] ->
            failwith "ScopeTable: global scope already closed, something fucked up"
        | glob::[] ->
            failwith "ScopeTable: tried closing global scope"
        | top::new_top::rest ->
            set ({new_top with label_num = top.label_num}::rest)

let generate_label =
    let* top::rest = get in
        let num = top.label_num in
            let* () = set ({top with label_num = 1 + top.label_num}::rest) in
                return @@ "B"^(string_of_int num)

let current_scope_size =
    let* top::rest = get in
        return top.current_scope_size

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

let rec local_lookup id state : location option = 
    match state with
    | [] ->
        None
    | glob::[] ->
        None
    | loc::rest ->
        begin match List.assoc_opt id loc.local_env with
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

let rec global_lookup id state =
    match state with
    | [] ->
        None
    | glob::[] ->
        if List.mem id glob.global_env
        then Some (GlobalLoc id)
        else None

let lookup id = 
    let* state = get in
        match local_lookup id state with
        | None ->
            begin match global_lookup id state with
            | Some location -> return location
            | None          -> fail @@ "unbound variable "^id
            end
        | Some location ->
            return location
