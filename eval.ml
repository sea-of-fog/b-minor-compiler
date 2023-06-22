(* Simple, recursive evaluator. Will be used to check
 * if the assembler compiled correctly.
 * TODO in the future: add monadic error catching
 *)

let rec eval exp =
    match exp with
    | NumE n -> n
    | OpE (AddO, e1, e2) -> (eval e1) + (eval e2)
    | OpE (SubO, e1, e2) -> (eval e1) - (eval e2)
    | OpE (MulO, e1, e2) -> (eval e1) * (eval e2)
    | OpE (DivO, e1, e2) -> (eval e1) / (eval e2)
