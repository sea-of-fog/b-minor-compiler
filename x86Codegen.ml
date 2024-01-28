open Syntax

let extract_location =
    Addressing.extract_location

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
    Code.single_line ("MOVQ "^(resolve mem1)^", "^(resolve mem2))

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
    | NumAE((mem, _), n)                -> move_const mem n
    | VarAE(mem, _)                     -> Code.empty
    | TrueAE(mem, _)                    -> move_const mem (-1)
    | FalseAE(mem, _)                   -> move_const mem 0
    | OpAE((mem, _), Add, exp1, exp2)   -> generic_operator "ADDQ" exp1 exp2 mem
    | OpAE((mem, _), Sub, exp1, exp2)   -> generic_operator "SUBQ" exp1 exp2 mem
    | OpAE((mem, _), And, exp1, exp2)   -> generic_operator "ANDQ" exp1 exp2 mem
    | OpAE((mem, _), Or, exp1, exp2)    -> generic_operator "ORQ " exp1 exp2 mem
    | OpAE((mem, _), Mul, exp1, exp2)   -> failwith "not implemented"
    | OpAE((mem, _), Div, exp1, exp2)   -> failwith "not implemented"
    | AssAE((mem, _), exp)              -> move mem (extract_location exp)

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
        Code.single_line ("."^bdata.label_v2^":")
        |> Code.concat @@ prog_codegen_helper ss Code.empty
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
    let var_names = let BlockAS(bdata, stmts)::[] = prog in
                    stmts
                    |> List.filter is_decl
                    |> List.map extract_var_name in
    let prog_code = prog_codegen_helper prog Code.empty in
    let decl_code = make_declarations var_names in
       Code.concat  (decl_code
                    |> Code.prefix ".data\n")
                    (Code.add_line "RET" prog_code
                    |> Code.prefix "main: \n"
                    |> Code.prefix "\t.global main\n"
                    |> Code.prefix ".text\n")

let program_codegen prog =
    Code.to_lines (prog_code prog)
