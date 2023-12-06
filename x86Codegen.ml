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
let rec expr_codegen (exp : Syntax.expr) table =
    match exp with
    | VarE(id) -> let free, new_table = scratch_alloc table in
                    (Code.single_line ("MOVQ ["^id^"], "^free),
                    new_table,
                    free)
    | NumE(n) -> let free, new_table = scratch_alloc table
                 in (Code.single_line ("MOVQ $"^(string_of_int n)^", "^free),
                     new_table,
                     free)
    | TrueE -> let free, new_table = scratch_alloc table
                 in (Code.single_line ("MOVQ $(-1), "^free),
                     new_table,
                     free)
    | FalseE -> let free, new_table = scratch_alloc table
                 in (Code.single_line ("MOVQ $0, "^free),
                     new_table,
                     free)
    | OpE(Add, exp1, exp2) -> let left_code, left_table, left_res = expr_codegen exp1 table in
                              let right_code, right_table, right_res = expr_codegen exp2 left_table in
                              ( Code.concat left_code right_code
                                |> Code.add_line ("ADDQ "^left_res^", "^right_res),
                                scratch_free left_res right_table,
                                right_res)
    | OpE(Sub, exp1, exp2) -> let left_code, left_table, left_res = expr_codegen exp1 table in
                              let right_code, right_table, right_res = expr_codegen exp2 left_table in
                              ( Code.concat left_code right_code
                                |> Code.add_line ("SUBQ "^right_res^", "^left_res),
                                scratch_free right_res right_table,
                                left_res)
    | OpE(Mul, exp1, exp2) -> let left_code, left_table, left_res = expr_codegen exp1 table in
                              let right_code, right_table, right_res = expr_codegen exp2 left_table in
                              ( Code.concat left_code right_code
                                |> Code.add_line ("MOVQ "^left_res^", %rax")
                                |> Code.add_line ("IMUL "^right_res)
                                |> Code.add_line ("MOVQ %rax, "^right_res),
                                scratch_free left_res right_table,
                                right_res)
    | OpE(And, exp1, exp2) -> let left_code, left_table, left_res = expr_codegen exp1 table in
                              let right_code, right_table, right_res = expr_codegen exp2 left_table in
                              ( Code.concat left_code right_code
                                |> Code.add_line ("ANDQ "^left_res^", "^right_res),
                                scratch_free left_res right_table,
                                right_res)
    | OpE(Or, exp1, exp2) -> let left_code, left_table, left_res = expr_codegen exp1 table in
                              let right_code, right_table, right_res = expr_codegen exp2 left_table in
                              ( Code.concat left_code right_code
                                |> Code.add_line ("ORQ "^left_res^", "^right_res),
                                scratch_free left_res right_table,
                                right_res)

let stmt_codegen (stmt : Syntax.stmt) : Code.t =
    match stmt with
    | AssS(id, exp) -> let (exp_code, _, res_register) = expr_codegen exp scratch_table in
                           Code.add_line ("MOVQ "^res_register^", ["^id^"]") exp_code
    | PrintS(exp, Some typ) -> 
        match typ with
        | IntT ->
            let (exp_code, _, res_register) = expr_codegen exp scratch_table in
                exp_code
                |> Code.add_line ("MOVQ  "^res_register^", %rdi")
                |> Code.add_line "XOR   %eax, %eax"
                |> Code.add_line "PUSHQ %r10"
                |> Code.add_line "PUSHQ %r11"
                |> Code.add_line "CALL  print_int"
                |> Code.add_line "POPQ  %r11";
                |> Code.add_line "POPQ  %r10"
        | BoolT ->
            let (exp_code, _, res_register) = expr_codegen exp scratch_table in
                exp_code
                |> Code.add_line ("MOVQ  "^res_register^", %rdi")
                |> Code.add_line "XOR   %eax, %eax"
                |> Code.add_line "PUSHQ %r10"
                |> Code.add_line "PUSHQ %r11"
                |> Code.add_line "CALL  print_bool"
                |> Code.add_line "POPQ  %r11";
                |> Code.add_line "POPQ  %r10"

let decl_codegen (decl : Syntax.decl) : Code.t =
    match decl with
    | SimpDec(id, typ, exp) -> let (exp_code, _, res_register) = expr_codegen exp scratch_table in
                                   Code.add_line ("MOVQ "^res_register^", ["^id^"]") exp_code 

let instr_codegen (instr : Syntax.instr) : Code.t =
    match instr with
    | Decl(decl) -> decl_codegen decl
    | Stmt(stmt) -> stmt_codegen stmt
    | Expr(exp)  -> let (code, _, _) = expr_codegen exp scratch_table in
                        code 

let rec prog_codegen_helper (prog : Syntax.prog) (acc : Code.t) : Code.t =
    match prog with
    | instr::rest -> let code = instr_codegen instr in
                        prog_codegen_helper rest (Code.concat acc code)
    | [] -> acc

let is_decl instr =
    match instr with
    | Decl(_) -> true
    | _ -> false

let extract_var_name instr =
    match instr with
    | Decl(SimpDec(id, typ, exp)) -> id

(* integers and initialized as 0 *)
let make_declarations (var_names : string list) : Code.t =
    Code.from_list (List.map (fun id -> id^": .quad 0") var_names)
    
let prog_code (prog : Syntax.prog) : Code.t =
    let var_names = prog
                    |> List.filter is_decl
                    |> List.map extract_var_name
    in let prog_code = prog_codegen_helper prog Code.empty 
    in let decl_code = make_declarations var_names in
        Code.concat 
                    (decl_code
                    |> Code.prefix "\tformat: .asciz \"%d\\n\"\n"
                    |> Code.prefix ".data\n")

                    (Code.add_line "RET" prog_code
                    |> Code.prefix "main: \n"
                    |> Code.prefix "\t.global main\n"
                    |> Code.prefix ".text\n")

let program_codegen prog =
    Code.to_lines (prog_code prog)
