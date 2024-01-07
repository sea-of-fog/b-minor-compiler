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

let rec instr ts =
    (((expr >> (fun e -> Expr e)) 
    <|> (stmt >> (fun s -> Stmt s))
    <|> (decl >> (fun d -> Decl d))) ++ semicolon) ts

and decl ts =
    (liftA6 _let id colon typ equal expr (fun _ (Id x) _ typ _ expr -> SimpDec(x, typ, expr))) ts

and stmt ts =
    ((((kword Print) ++ expr) >> (fun (_, expr) -> PrintS(expr, None)))
 <|>((id ++ equal ++ expr) >> (fun (((Id x), _), expr) -> AssS(x, expr)))) ts

and expr ts =
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
 <|>((open_paren ++ expr ++ closed_paren) >> (fun ((_, (e : expr)), _) -> e))
 <|>(id >> (fun (Id x) -> (VarE x)))) ts

let program : (prog pars)  = many instr

let program_parser ts =
    match program ts with
    | Some (prog, rest) -> prog
    | _                 -> failwith "could not parse program"
