module M = Map.Make(String)

let scratch_table =
    [("%rbx", false); ("%r10", false); ("%r11", false); ("%r12", false); ("%r13", false); ("%r14", false); ("%r15", false)]
    |> List.to_seq
    |> M.of_seq

let scratch_free reg_name table =
    let f x = match x with
              | None -> None
              | Some b -> Some false
        in M.update reg_name f table

let scratch_alloc table =
    let free_reg = table
                   |> M.filter (fun _ x -> not x)
                   |> M.min_binding
                   |> fst
        in (free_reg, M.update free_reg
                               (fun x -> match x with
                                         | None -> None
                                         | Some x -> Some true)
                               table)
