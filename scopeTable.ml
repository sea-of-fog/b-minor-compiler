open Syntax

type env = (string * location) list

type st = {
    global              : (string * location) list;
    num_of_scopes       : int;
    current_scope_size  : int;
    combined_scope_size : int;
}

open ErrorStateMonad.Make()
