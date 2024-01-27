open Syntax

let resolve mem =
    failwith "not implemented"

let move_const mem n =
    Code.add_line ("MOVQ ")

let rec expr_codegen exp = function
    | NumAE((mem, _), n) ->
        move_const mem n
    | VarAE(mem, _) ->
        Code.Nil
    | TrueAE(mem, _) ->
        move_const mem (-1)
    | FalseAE(mem, _) ->
        move_const mem 0

let rec expr_codegen (exp : Syntax.expr) table : (Code.t * table * string)=
    match exp with
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
    | AssE(id, exp) -> let (exp_code, table, res_register) = expr_codegen exp scratch_table in
                           (Code.add_line ("MOVQ "^res_register^", ["^id^"]") exp_code,
                            table,
                            res_register)

let decl_codegen (decl : Syntax.decl) : Code.t =
    match decl with
    | SimpDec(id, typ, exp) -> let (exp_code, _, res_register) = expr_codegen exp scratch_table in
                                   Code.add_line ("MOVQ "^res_register^", ["^id^"]") exp_code 

let stmt_codegen (stmt : Syntax.stmt) : Code.t =
    match stmt with
    | DeclS decl -> decl_codegen decl
    | PrintS(exp, Some typ) -> 
        begin match typ with
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
        end
    | ExprS exp   -> let (code, _, _) = expr_codegen exp scratch_table in
                        code 

let rec prog_codegen_helper (prog : Syntax.prog) (acc : Code.t) : Code.t =
    match prog with
    | stmt::rest -> let code = stmt_codegen stmt in
                        prog_codegen_helper rest (Code.concat acc code)
    | [] -> acc

let is_decl stmt =
    match stmt with
    | DeclS(_) -> true
    | _        -> false

let extract_var_name stmt =
    match stmt with
    | DeclS(SimpDec(id, typ, exp)) -> id

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
