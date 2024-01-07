open Syntax
open ParseLib

let number : (Syntax.expr ParseLib.pars) =
    function
    | (Number n)::ts -> Some (NumE n, ts)
    | _              -> None

let add ts =
    const (Op Add) Add

let sub =
    const (Op Sub) Sub

let mul ts =
    const (Op Mul) Mul

let div =
    const (Op Div) Div

let open_paren =
    const OpenParen ()

let closed_paren =
    const ClosedParen ()

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
