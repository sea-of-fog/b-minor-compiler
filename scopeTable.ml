open Syntax

module State = struct
    type env = (string * location) list
    type t = { 
        global_env          : env;
        local_env_stack     : env list;
        current_scope_size  : int;
        label_num           : int;
    }
end

module M = ErrorStateMonad.Make(State)
open M

type 'a res = 
    | Ok of 'a
    | Fail of string

type 'a t = 
    'a M.t

let starting_state = 
   {global_env = [];
    local_env_stack = [];
    current_scope_size [];
    label_num = 0 }

let return = M.return
let ( let* ) = M.( let* )
let run comp = 

let fail = M.fail

let add_to_current_scope id =
    let* state = get in
        match state.local_env_stack with
        | [] ->
            begin match List.assoc_opt id state.global_env with
            | None -> 
                let* () = set { global_env = (id, Global id)::state.global_env;
                                local_env_stack = state.local_env_stack;
                                current_scope_size = 1 + state.current_scope_size;
                                label_num = state.label_num
                              } in
                    return @@ Global id
            | Some _ ->
                fail @@ "variable \""^id^"\" is already bound in global scope"
            end
        | env::rest ->
            begin match List.assoc_opt id env with
            | None -> 
                let pos = state.current_scope_size + 1 in
                    let* () = set { global_env = (id, Global id)::state.global_env;
                                    local_env_stack = state.local_env_stack;
                                    current_scope_size = pos;
                                    label_num = state.label_num
                                  } in
                        return @@ Local { scope = 0; pos = pos}
            | Some _ ->
                fail @@ "variable \""^id^"\" is already bound in global scope"
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
        | []::rest ->
            set { local_env_stack = []::(state.local_env_stack);
                  global_env = state.global_env;
                  current_scope_size = 0;
                  label_num = state.label_num}     
        | _::rest ->
            failwith "ScopeTable: tried closing nonempty scope"
        | [] ->
            failwith "Tried closing global scope"

let generate_label =
    let* state = get in
        let num = state.label_num in
            let* () = set { local_env_stack = state.local_env_stack;
                            global_env = state.global_env;
                            current_scope_size = state.current_scope_size;
                            label_num = num + 1
                          } in 
                return @@ ".B"^(string_of_int num)

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

let rec local_lookup id stack : location option = 
    match stack with
    | [] ->
        None
    | env::rest ->
        begin match List.assoc id env with
        | Some ind ->
          Some (Local { pos = List.length env - ind;
            scope = 0
          })
        | None ->
            begin match local_lookup id rest with
            | None ->
                None
            | Some (Local { pos; scope }) ->
                Some (Local { pos; scope = scope + 1 })
            end
        end

let lookup id = 
    failwith "not implemented"
