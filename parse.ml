open Syntax
open ParseLib

let number : (Syntax.expr ParseLib.pars) =
    function
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
    | ClosedParen::ts -> Some((), ts)
    | _               -> None

let rec expr ts =
    ((term ++ expr_prime) >> (fun (e, k) -> k e)) ts

and expr_prime ts =
    (((add ++ term ++ expr_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
 <|>((sub ++ term ++ expr_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
 <|>((eps (fun e -> e)))) ts

and term ts = 
    ((atom ++ term_prime) >> (fun (e, k) -> k e)) ts

and term_prime ts =
    ((((mul ++ atom) ++ term_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
 <|>((div ++ atom ++ term_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
 <|>((eps (fun e -> e)))) ts

and atom ts =
    (number 
 <|>((open_paren ++ expr ++ closed_paren) >> (fun ((_, (e : expr)), _) -> e))) ts
