open Syntax
open ParseLib

let number : (Syntax.expr ParseLib.pars) =
    function
    | (Number n)::ts -> Some (NumE n, ts)
    | _              -> None

let id = function
    | (Id x)::ts -> Some ((Id x), ts)
    | _          -> None

let _let =
    symbol (Keyword Let)

let colon = 
    symbol Colon

let semicolon = 
    symbol SemiColon

let equal = 
    symbol Equal

let add =
    const (Op Add) Add

let sub =
    const (Op Sub) Sub

let mul =
    const (Op Mul) Mul

let div =
    const (Op Div) Div

let open_paren =
    const OpenParen ()

let closed_paren =
    const ClosedParen ()

let typ =
    (const (Keyword Int) IntT) <|> 
    (const (Keyword Bool) BoolT)

let rec stmt ts = 
    ((((((kword Print) ++ expr) >> (fun (_, expr) -> PrintS(expr, None)))
<|> (decl >> (fun d -> DeclS d))
<|> (expr >> (fun e -> ExprS e))) ++ semicolon) >> ((fun (s, _) -> s))) ts

and decl ts =
    (liftA6 _let id colon typ equal expr (fun _ (Id x) _ typ _ expr -> SimpDec(x, typ, expr))) ts

and expr ts =
    ass_expr ts

and ass_expr ts =
    ((id ++ equal ++ expr) >> (fun (((Id x), _), expr) -> AssE(x, expr))
<|> or_expr) ts

and or_expr ts =
    ((liftA3 and_expr (symbol @@ Op Or) or_expr (fun e1 _ e2 -> OpE(Or, e1, e2)))
<|> and_expr) ts

and and_expr ts =
    ((liftA3 ar_expr (symbol @@ Op And) and_expr (fun e1 _ e2 -> OpE(And, e1, e2)))
<|> ar_expr) ts

and ar_expr ts =
    ((term ++ ar_expr_prime) >> (fun (e, k) -> k e)) ts

and ar_expr_prime ts =
    (((add ++ term ++ ar_expr_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
<|> ((sub ++ term ++ ar_expr_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
<|> ((eps (fun e -> e)))) ts

and term ts = 
    ((atom ++ term_prime) >> (fun (e, k) -> k e)) ts

and term_prime ts =
    ((((mul ++ atom) ++ term_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
<|> ((div ++ atom ++ term_prime) >> (fun ((op, e), k) -> (fun t -> k @@ OpE(op, t, e))))
<|> ((eps (fun e -> e)))) ts

and atom ts =
    (number 
<|> ((open_paren ++ expr ++ closed_paren) >> (fun ((_, (e : expr)), _) -> e))
<|> (id >> (fun (Id x) -> (VarE x)))
<|> ((kword False) >> (fun _ -> FalseE))
<|> ((kword True)  >> (fun _ -> TrueE))) ts

let program : (prog pars)  = many stmt 

let program_parser ts =
    match program ts with
    | Some (prog, rest) -> prog
    | _                 -> failwith "could not parse program"
