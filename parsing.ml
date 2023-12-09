open Syntax
open ParseLib

let number ts =
    match ts with
    | (Number n)::ts -> Some (NumE n, ts)
    | _              -> None

let add ts =
    match ts with
    | (Op Add)::ts -> Some(Add, ts)
    | _            -> None

let sub ts =
    match ts with
    | (Op Add)::ts -> Some(Sub, ts)
    | _            -> None

let mul ts =
    match ts with
    | (Op Mul)::ts -> Some(Add, ts)
    | _            -> None

let div ts =
    match ts with
    | (Op Div)::ts -> Some(Sub, ts)
    | _            -> None

let open_paren ts =
    match ts with
    | OpenParen::ts -> Some((), ts)
    | _             -> None

let closed_paren ts =
    match ts with
    | ClosedPare::ts -> Some((), ts)
    | _              -> None

let rec expr =
    (term ++ expr_prime) >> (fun (e, k) -> k e)

and expr_prime =
    ((add ++ term ++ expr_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
 <|>((sub ++ term ++ expr_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
 <|>((eps (fun e -> e)))

and term = 
    (atom ++ term_prime) >> (fun (e, k) -> k e)

and term_prime =
    ((mul ++ atom ++ term_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
 <|>((div ++ atom ++ term_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
 <|>((eps (fun e -> e)))

and atom =
    number
 <|>((open_paren ++ expr ++ closed_paren) >> (fun ((_, e), _) -> e))
