open Syntax

module M = Map.Make(String)

let scratch_table =
    [("%r10", false); ("%r11", false); ("%r12", false); ("%r13", false); ("%r14", false); ("%r15", false)]
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

(* zwraca kod, tablicę oraz rejestr, w którym jest wynik wyrażenia *) 
let rec expr_codegen exp table =
    match exp with
    | VarE(id) -> let free, new_table = scratch_alloc table in
                    ((("MOVQ ["^id^"], "^free)::[]),
                    new_table,
                    free)
    | NumE(n) -> let free, new_table = scratch_alloc table
                 in ( ("MOVQ $"^(Printf.sprintf "%d" n)^", "^free)::[],
                     new_table,
                     free)
    | TrueE -> let free, new_table = scratch_alloc table
                 in ( ("MOVQ $(-1), "^free)::[],
                     new_table,
                     free)
    | FalseE -> let free, new_table = scratch_alloc table
                 in ( ("MOVQ $0, "^free)::[],
                     new_table,
                     free)
    | OpE(Add, exp1, exp2) -> let left_code, left_table, left_res = expr_codegen exp1 table in
                              let right_code, right_table, right_res = expr_codegen exp2 left_table in
                              ( ("ADDQ "^left_res^", "^right_res)::(right_code@left_code),
                                scratch_free left_res right_table,
                                right_res)
    | OpE(Sub, exp1, exp2) -> let left_code, left_table, left_res = expr_codegen exp1 table in
                              let right_code, right_table, right_res = expr_codegen exp2 left_table in
                              ( ("SUBQ "^right_res^", "^left_res)::(right_code@left_code),
                                scratch_free right_res right_table,
                                left_res)
    | OpE(Mul, exp1, exp2) -> let left_code, left_table, left_res = expr_codegen exp1 table in
                              let right_code, right_table, right_res = expr_codegen exp2 left_table in
                              ( ("MOVQ %rax, "^right_res) ::
                                ("IMUL "^right_res) ::
                                ("MOVQ "^left_res^", %rax") ::
                                (right_code@left_code),
                                scratch_free left_res right_table,
                                right_res)

let stmt_codegen stmt =
    match stmt with
    | AssS(id, exp) -> let (exp_code, _, res_register) = expr_codegen exp scratch_table in
        let new_code = "MOVQ "^res_register^", ["^id^"]" in
            exp_code@[new_code]
    | PrintS(exp)   -> let (exp_code, _, res_register) = expr_codegen exp scratch_table in
                            let new_code = ["MOVQ  $format, %rdi"; 
                                            "MOVQ  "^res_register^" , %rsi"; 
                                            "MOVQ  $0";
                                            "PUSHQ %r10";
                                            "PUSHQ %r11";
                                            "CALL  printf";
                                            "POPQ  %r11";
                                            "POPQ  %r10"] in
                                exp_code@new_code

let decl_codegen decl =
    match decl with
    | SimpDec(id, typ, exp) -> let (exp_code, _, res_register) = expr_codegen exp scratch_table in
                                   let new_code = "MOVQ "^res_register^", ["^id^"]" in
                                       exp_code@(new_code::[])

let instruction_codegen instr =
    match instr with
    | Decl(decl) -> decl_codegen decl
    | Stmt(stmt) -> stmt_codegen stmt
    | Expr(exp)  -> let (code, _, _) = expr_codegen exp scratch_table in (List.rev code)

let rec program_codegen_helper prog acc =
    match prog with
    | instr::rest -> let code = instruction_codegen instr in
                        program_codegen_helper rest (acc@code)
    | [] -> acc

let is_decl instr =
    match instr with
    | Decl(_) -> true
    | _ -> false

let extract_var_name instr =
    match instr with
    | Decl(SimpDec(id, typ, exp)) -> id

(* integers initialized as 0 *)
let make_declarations (var_names : string list) : string list =
    (List.map (fun id -> id^": .quad 0") var_names)
    

let program_codegen prog =
    let var_names = prog
                    |> List.filter is_decl
                    |> List.map extract_var_name
    in let program_code = program_codegen_helper prog []
    in let decl_code = make_declarations var_names in
        [".data\n";
         "\tformat: .asciz \"%d\\n\"\n"]@
        decl_code@
        [".text\n";
        "\t.global main\n"; "main: \n"]@
        program_code@
        ["RET"]
