(* Simple, recursive evaluator. Will be used to check
 * if the assembler compiled correctly.
 * TODO in the future: add monadic error catching
 *)

let rec eval exp =
    match exp with
    | NumE n -> n
    | OpE (Add, e1, e2) -> (eval e1) + (eval e2)
    | OpE (Sub, e1, e2) -> (eval e1) - (eval e2)
    | OpE (Mul, e1, e2) -> (eval e1) * (eval e2)
    | OpE (Div, e1, e2) -> (eval e1) / (eval e2)
