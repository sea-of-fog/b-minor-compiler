open Syntax

let extract_location =
    Addressing.extract_location

let max x y =
    if x < y then y else x

let rec max_temps = function
    | ExprAS _ ->
        0
    | DeclAS _ ->
        0
    | PrintAS _ ->
        0
    | BlockAS (bdata, stmts) ->
        max bdata.temp_vars_v2
            (List.fold_left max 0 (List.map max_temps stmts))
    | IfAS (exp, stmt) ->
            max_temps stmt
    | WhileAS (exp, stmt) ->
            max_temps stmt

let rec max_local = function
    | ExprAS _ ->
        0
    | DeclAS _ ->
        0
    | PrintAS _ ->
        0
    | BlockAS (bdata, stmts) ->
        bdata.local_vars_v2 + (List.fold_left max 0 (List.map max_local stmts))
    | IfAS (exp, stmt) ->
            max_local stmt
    | WhileAS (exp, stmt) ->
            max_local stmt

let comp_box = 
    ref 0

let generate_comparison_label () =
        let cnt = !comp_box in
            comp_box := cnt + 1; (".COMP_TRUE"^(string_of_int cnt), ".COMP_ESC"^(string_of_int cnt))

let if_box = 
    ref 0

let generate_if_label () =
        let cnt = !if_box in
            if_box := cnt + 1; ".IF_ESC"^(string_of_int cnt)

let while_box = 
    ref 0

let generate_while_label () =
        let cnt = !while_box in
            while_box := cnt + 1; (".WHILE_ESC"^(string_of_int cnt), ".WHILE_LOOP"^(string_of_int cnt))

let resolve_reg = function
    | RBX -> "%rbx"
    | R09 -> "%r9"
    | R10 -> "%r10"
    | R11 -> "%r11"
    | R12 -> "%r12"
    | R13 -> "%r13"
    | R14 -> "%r14"
    | R15 -> "%r15"

(* watch out whether stack offsets start from 0 or 1! *)
let resolve = function
    | RegisterMem r -> resolve_reg r
    | GlobalMem id  -> "["^id^"]"
    | LocalMem ind  -> "-"^(string_of_int (8*ind))^"(%rbp)"
    | TempMem ind   -> "-"^(string_of_int (8*ind))^"(%rbp)"

let move_const mem n =
    Code.single_line ("MOVQ $("^(string_of_int n)^"), "^(resolve mem))

let move mem1 mem2 =
    Code.single_line ("MOVQ "^(resolve mem1)^", %rax")
    |> Code.add_line ("MOVQ %rax, "^(resolve mem2))

(* Checks whether the values are already registerized,
   if not, registerizes them to rbx and r09 respectively *)
let generic_operator instr exp1 exp2 tgt =
    match extract_location exp1 with
    | RegisterMem r1 ->
        begin match extract_location exp2 with
        | RegisterMem r2 ->
            Code.single_line (instr^" "^(resolve_reg r1)^", "^(resolve_reg r2))
        | mem2 ->
            Code.concat (move mem2 (RegisterMem RBX)
                         |> Code.add_line (instr^" "^(resolve_reg r1)^", "^(resolve_reg RBX)))
                        (move (RegisterMem RBX) tgt) 
        end
    | mem1 ->
        begin match extract_location exp2 with
        | RegisterMem r2 ->
            move mem1 (RegisterMem RBX)
            |> Code.add_line (instr^" "^(resolve_reg RBX)^", "^(resolve_reg r2))
        | mem2 ->
            Code.concat (Code.concat (move mem1 (RegisterMem RBX))
                                     (move mem2 (RegisterMem R09))
                        |> Code.add_line (instr^" "^(resolve_reg RBX)^", "^(resolve_reg R09)))
                        (move (RegisterMem R09) tgt)
        end

let op_to_string = function
    | Add -> "ADDQ"
    | Sub -> "SUBQ"
    | Or  -> "ORQ "
    | And -> "ANDQ"
    | Mul -> "IMULQ"
    | Div -> "IDIVQ"
    | _   -> failwith "shouldn't have tried to turn this operator to string"
    
let jump_of_op = function
    | Lt  -> "JL   "
    | Gt  -> "JG   "
    | Leq -> "JLE  "
    | Geq -> "JGE  "
    | Eq  -> "JE   "
    | Neq -> "JNE  "

let rec expr_codegen = function
    | NumAE((mem, _), n) ->
        move_const mem n
    | VarAE(mem, _) ->
        Code.empty
    | TrueAE(mem, _) ->
        move_const mem (-1)
    | FalseAE(mem, _) ->
        move_const mem 0
    | OpAE((mem, _), ((Add|Sub|And|Or) as op), exp1, exp2) -> 
        Code.concat (Code.concat (expr_codegen exp1)
                                 (expr_codegen exp2))
                    (generic_operator (op_to_string op) exp2 exp1 mem)
    | OpAE((mem, _), Mul, exp1, exp2) ->
        Code.concat (Code.concat (expr_codegen exp1)
                                 (expr_codegen exp2))
                    (Code.single_line "PUSHQ %rdx"
                     |> Code.add_line ("MOVQ "^(resolve @@ extract_location exp1)^", %rax")
                     |> Code.add_line ("IMULQ "^(resolve @@ extract_location exp2))
                     |> Code.add_line ("MOVQ %rax, "^(resolve mem))
                     |> Code.add_line "POPQ %rdx")
    | OpAE((mem, _), Div, exp1, exp2) ->
        Code.concat (Code.concat (expr_codegen exp1)
                                 (expr_codegen exp2))
                    (Code.single_line "PUSHQ %rdx"
                     |> Code.add_line ("MOVQ "^(resolve @@ extract_location exp1)^", %rax")
                     |> Code.add_line "CQO"
                     |> Code.add_line ("IDIVQ "^(resolve @@ extract_location exp2))
                     |> Code.add_line ("MOVQ %rax, "^(resolve mem))
                     |> Code.add_line "POPQ %rdx")
    | OpAE((mem, _), ((Lt|Gt|Leq|Geq|Eq|Neq) as comp_op), exp1, exp2) ->
        let (true_label, escape_label) = generate_comparison_label () in
            let instr = jump_of_op comp_op in
                Code.concat (Code.concat (expr_codegen exp1) 
                                         (expr_codegen exp2))
                            (((Code.concat (move (extract_location exp1) (RegisterMem RBX))
                                           (move (extract_location exp2) (RegisterMem R09)))
                            |> Code.add_line "CMPQ %r9, %rbx"
                            |> Code.add_line (instr^"  "^true_label)
                            |> Code.rev_concat (move_const mem 0)
                            |> Code.add_line ("JMP  "^escape_label)
                            |> Code.add_line (true_label^":")
                            |> Code.rev_concat (move_const mem (-1))
                            |> Code.add_line (escape_label^":"))
                            |> Code.prefix "# comparison starting"
                            |> Code.add_line "# comparison ending")
    | AssAE((mem, _), exp) ->
        Code.concat (expr_codegen exp)
                    (move (extract_location exp) mem)

let decl_codegen decl =
    match decl with
    | SimpADec((mem, _), _, exp) ->
        (Code.concat @@ expr_codegen exp)
                    @@ move (extract_location exp) mem

let extract_type exp =
    snd @@ Addressing.extract_data exp

let rec stmt_codegen stmt : Code.t =
    match stmt with
    | DeclAS decl -> 
        decl_codegen decl
    | ExprAS exp ->
        expr_codegen exp
    | PrintAS exp -> 
        begin match extract_type exp with
        | IntT ->
            let exp_code = expr_codegen exp in
                exp_code
                |> Code.add_line ""
                |> Code.add_line "# printing integer"
                |> Code.add_line ("MOVQ  "^(resolve @@ extract_location exp)^", %rdi")
                |> Code.add_line "XOR   %eax, %eax"
                |> Code.add_line "PUSHQ %r10"
                |> Code.add_line "PUSHQ %r11"
                |> Code.add_line "CALL  print_int"
                |> Code.add_line "POPQ  %r11"
                |> Code.add_line "POPQ  %r10"
                |> Code.add_line ""
        | BoolT ->
            let exp_code = expr_codegen exp in
                exp_code
                |> Code.add_line ""
                |> Code.add_line "# printing boolean"
                |> Code.add_line ("MOVQ  "^(resolve @@ extract_location exp)^", %rdi")
                |> Code.add_line "XOR   %eax, %eax"
                |> Code.add_line "PUSHQ %r10"
                |> Code.add_line "PUSHQ %r11"
                |> Code.add_line "CALL  print_bool"
                |> Code.add_line "POPQ  %r11";
                |> Code.add_line "POPQ  %r10"
                |> Code.add_line ""
        end
    | BlockAS (bdata, ss) ->
        prog_codegen_helper ss Code.empty
        |> Code.prefix ("."^bdata.label_v2^":")
(* FIXME: add escape labels! *)
    | IfAS (exp, stmt) ->
        let escape_label = generate_if_label () in
        let exp_code = expr_codegen exp in
        let exp_mem = extract_location exp in
        let stmt_code = stmt_codegen stmt in
            (Code.concat (exp_code
                         |> Code.rev_concat (move exp_mem (RegisterMem RBX))
                         |> Code.add_line "MOVQ $(0), %r9"
                         |> Code.add_line "CMPQ %rbx, %r9"
                         |> Code.add_line ("JE  "^escape_label))
                        stmt_code)
            |> Code.add_line (escape_label^":")
    | WhileAS (exp, stmt) ->
        let (escape_label, loop_label) = generate_while_label () in
        let exp_code = expr_codegen exp in
        let exp_mem = extract_location exp in
        let stmt_code = stmt_codegen stmt in
            Code.single_line "# while loop"
            |> Code.add_line (loop_label^":")
            |> Code.add_line "# evaluating condition"
            |> Code.rev_concat exp_code
            |> Code.rev_concat (move exp_mem (RegisterMem RBX))
            |> Code.add_line "MOVQ $(0), %r9"
            |> Code.add_line "CMPQ %rbx, %r9"
            |> Code.add_line ("JE  "^escape_label)
            |> Code.rev_concat stmt_code
            |> Code.add_line ("JMP  "^loop_label)
            |> Code.add_line (escape_label^":")
(*TODO: Add type tests for ifs! *)

and prog_codegen_helper prog acc : Code.t =
    match prog with
    | stmt::rest -> let code = stmt_codegen stmt in
                        prog_codegen_helper rest (Code.concat acc code)
    | [] -> acc

let is_decl = function
    | DeclAS(_) -> true
    | _        -> false

let extract_var_name = function
    | DeclAS(SimpADec((GlobalMem id, _), _, _)) -> id

(* integers are initialized as 0 *)
let make_declarations (var_names : string list) : Code.t =
    Code.from_list (List.map (fun id -> id^": .quad 0") var_names)

let increase_mem n mem =
    match mem with
    | LocalMem ind -> LocalMem (ind + n)
    | mem          -> mem

let rec increase_exp exp n =
    match exp with
    | NumAE((mem, typ), l)              -> NumAE((increase_mem n mem, typ), l) 
    | TrueAE (mem, typ)                 -> TrueAE(increase_mem n mem, typ)
    | FalseAE (mem, typ)                -> FalseAE(increase_mem n mem, typ)
    | VarAE (mem, typ)                  -> VarAE(increase_mem n mem, typ)
    | AssAE((mem, typ), exp)            -> AssAE((increase_mem n mem, typ), increase_exp exp n)
    | OpAE((mem, typ), op, exp1, exp2)  -> OpAE((increase_mem n mem, typ), op, increase_exp exp1 n, increase_exp exp2 n)

let increase_decl decl n =
    match decl with
    | SimpADec ((mem, typ1), typ2, exp) -> SimpADec ((increase_mem n mem, typ1), typ2, increase_exp exp n)

let rec increase_locals_stmt n =
    function
    | ExprAS exp  -> ExprAS  (increase_exp exp n)
    | PrintAS exp -> PrintAS (increase_exp exp n)
    | DeclAS d    -> DeclAS  (increase_decl d n)
    | BlockAS (bdata, stmts) -> BlockAS (bdata, List.map (increase_locals_stmt n) stmts)
    | IfAS (exp, stmt) -> IfAS (increase_exp exp n, increase_locals_stmt n stmt)
    | WhileAS (exp, stmt) -> WhileAS (increase_exp exp n, increase_locals_stmt n stmt)
    
let prog_code prog : Code.t =
    let [global_block] = prog in
    let needed_temps = max_temps global_block in
    let needed_local = max_local global_block in
    let global_block = increase_locals_stmt needed_temps global_block in
    let var_names = let BlockAS(_, stmts) = global_block in
                    stmts
                    |> List.filter is_decl
                    |> List.map extract_var_name in
    let prog = [global_block] in
    let prog_code = prog_codegen_helper prog Code.empty in
    let decl_code = make_declarations var_names in
       Code.concat  (decl_code
                    |> Code.prefix ".data\n")
                    ((prog_code
                    |> Code.prefix ""
                    |> Code.prefix ("SUBQ $("^(string_of_int (8*(needed_temps + needed_local)))^"), %rsp")
                    |> Code.prefix "MOVQ %rsp, %rbp"
                    |> Code.prefix "PUSHQ %rbp"
                    |> Code.prefix "# allocating local and temporary variables"
                    |> Code.prefix ""
                    |> Code.prefix "main:"
                    |> Code.prefix "\t.global main"
                    |> Code.prefix ".text")
                    |> Code.add_line "# restoring stack after main call"
                    |> Code.add_line "MOVQ %rbp, %rsp"
                    |> Code.add_line "POPQ %rbp"
                    |> Code.add_line "RET")

let program_codegen prog =
    Code.to_lines (prog_code prog)
