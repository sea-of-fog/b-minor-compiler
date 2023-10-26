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

let rec codegen exp table =
    match exp with
    | NumE(n) -> let free, new_table = scratch_alloc table
                 in ( ("MOVQ $"^(Printf.sprintf "%d" n)^", "^free)::[],
                     new_table,
                     free)
    | OpE(Add, exp1, exp2) -> let left_code, left_table, left_res = codegen exp1 table in
                              let right_code, right_table, right_res = codegen exp2 left_table in
                              ( ("ADDQ "^left_res^", "^right_res)::(right_code@left_code),
                                scratch_free left_res right_table,
                                right_res)
    | OpE(Sub, exp1, exp2) -> let left_code, left_table, left_res = codegen exp1 table in
                              let right_code, right_table, right_res = codegen exp2 left_table in
                              ( ("SUBQ "^right_res^", "^left_res)::(right_code@left_code),
                                scratch_free right_res right_table,
                                left_res)
    | OpE(Mul, exp1, exp2) -> let left_code, left_table, left_res = codegen exp1 table in
                              let right_code, right_table, right_res = codegen exp2 left_table in
                              ( ("MOVQ %rax, "^right_res) ::
                                ("IMUL "^right_res) ::
                                ("MOVQ "^left_res^", %rax") ::
                                (right_code@left_code),
                                scratch_free left_res right_table,
                                right_res)

module O = Out_channel

let rec output_lines channel lines =
    match lines with
    | [] -> ()
    | line::lines -> let () = O.output_string channel ("\t"^line^"\n")
                     in output_lines channel lines


let write_code tgt exp =
    let code, _, _ = codegen exp scratch_table in
    let channel = O.open_text tgt in
    output_lines channel (List.rev code)
