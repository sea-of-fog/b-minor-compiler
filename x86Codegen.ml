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

let rec max_local = function
    | ExprAS _ ->
        0
    | DeclAS _ ->
        0
    | PrintAS _ ->
        0
    | BlockAS (bdata, stmts) ->
        bdata.local_vars_v2 + (List.fold_left max 0 (List.map max_local stmts))

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

let rec expr_codegen = function
    | NumAE((mem, _), n) ->
        move_const mem n
    | VarAE(mem, _) ->
        Code.empty
    | TrueAE(mem, _) ->
        move_const mem (-1)
    | FalseAE(mem, _) ->
        move_const mem 0
    | OpAE((mem, _), Add, exp1, exp2) -> 
        Code.concat (Code.concat (expr_codegen exp1)
                                 (expr_codegen exp2))
                    (generic_operator "ADDQ" exp2 exp1 mem)
    | OpAE((mem, _), Sub, exp1, exp2) -> 
        Code.concat (Code.concat (expr_codegen exp1)
                                 (expr_codegen exp2))
                    (generic_operator "SUBQ" exp2 exp1 mem)
    | OpAE((mem, _), And, exp1, exp2) -> 
        Code.concat (Code.concat (expr_codegen exp1)
                                 (expr_codegen exp2))
                    (generic_operator "ANDQ" exp2 exp1 mem)
    | OpAE((mem, _), Or, exp1, exp2) -> 
        Code.concat (Code.concat (expr_codegen exp1)
                                 (expr_codegen exp2))
                    (generic_operator "ORQ" exp2 exp1 mem)
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
    
let prog_code prog : Code.t =
    let [global_block] = prog in
    let needed_temps = max_temps global_block in
    let needed_local = max_local global_block in
    let var_names = let BlockAS(_, stmts) = global_block in
                    stmts
                    |> List.filter is_decl
                    |> List.map extract_var_name in
    let prog_code = prog_codegen_helper prog Code.empty in
    let decl_code = make_declarations var_names in
       Code.concat  (decl_code
                    |> Code.prefix ".data\n")
                    ((prog_code
                    |> Code.prefix ""
                    |> Code.prefix ("SUBQ $("^(string_of_int (8*(needed_temps + needed_local)))^"), %rsp")
                    |> Code.prefix "# allocating local and temporary variables"
                    |> Code.prefix ""
                    |> Code.prefix "main:"
                    |> Code.prefix "\t.global main"
                    |> Code.prefix ".text")
                    |> Code.add_line "RET")

let program_codegen prog =
    Code.to_lines (prog_code prog)
