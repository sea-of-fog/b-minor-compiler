open Syntax

(* This version (v1) of the parser is based on the following CFG:
 * S -> E (as in "expression")
 * E -> E + T | E - T | T (as in "term")
 * T -> N | (E) (N stands for "number")
 * 
 * A few notes:
 * 1) here, we are operating on tokens, so whole numbers are 
 *    lexical terminals
 * 2) this grammar has left-recursive productions, so is not suitable
 *    for automatically generating parsers
 * 3) however, any valid parsing tree IS LEFT-ASSOCIATIVE
 * 
 * The implementation will be based on an alternative, right-recursive
 * CFG. The trick for eliminating left-recursion can be found in: 
 * (slides 3/4)
 * https://people.montefiore.uliege.be/geurts/Cours/compil/2011/03-syntaxanalysis-2.pdf
 * 
 * Another useful resource:
 * https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/Syntax.html/node8.html
 *
 * New:
 * S  -> E
 * E  -> TE'
 * E' -> +TE' | -TE' | \varepsilon
 * T  -> AT'
 * T' -> *AT' | /AT' | \varepsilon
 * A -> Number | (E)
 *
 * Notes on implementation: 
 * 1) since we are building left-associative expressions,
 *    the parsers for T' and S' will return lambdas waiting for the right term
 *)

(* the v2 version will deal with actual programs, not just expressions! *)
(* a program in v2 is a sequence of instructions. an instruction is a declaration (with let!), *)
(* an assignment statement or an expression *)

(* The grammar is now: *)

(* S -> I Colon S | varepsilon *)
(* I -> D | St | E *)
(* D -> Let Id Equal E *)
(* St -> Id Equal E | Print E *)
(* E -> as above *)
(* E  -> TE' *)
(* E' -> +TE' | -TE' | \varepsilon *)
(* T  -> AT' *)
(* T' -> *AT' | /AT' | \varepsilon *)
(* A -> Number | Id | (E) *)

let rec expr_parser ts =
    match term_parser ts with
    | None -> None
    | Some (exp, ts) -> match expr_prime_parser ts with
                        | None -> None
                        | Some (f, rest)  -> Some (f exp, rest)

and expr_prime_parser ts =
    match ts with
    | (Op (Add))::ts -> (match term_parser ts with
                         | None -> None
                         | Some (term, rest) -> match expr_prime_parser rest with
                                                | None -> None
                                                | Some (f, rest) -> Some ((fun e -> f (OpE (Add, e, term))), rest))
    | (Op (Sub))::ts -> (match term_parser ts with
                         | None -> None
                         | Some (term, rest) -> match expr_prime_parser rest with
                                                | None -> None
                                                | Some (f, rest) -> Some ((fun e -> f (OpE (Sub, e, term))), rest))
    | _ -> Some ((fun s -> s), ts)

and term_parser ts =
    match atom_parser ts with
    | None -> None
    | Some (exp, ts) -> match term_prime_parser ts with
                        | None -> None
                        | Some (f, rest) -> Some (f exp, rest)

and term_prime_parser ts =
    match ts with
    | (Op (Mul))::ts -> (match atom_parser ts with
                         | None -> None
                         | Some (atom, rest) -> match term_prime_parser rest with
                                                | None -> None
                                                | Some (f, rest) -> Some ((fun e -> f (OpE (Mul, e, atom))), rest))
    | (Op (Div))::ts -> (match atom_parser ts with
                         | None -> None
                         | Some (atom, rest) -> match term_prime_parser rest with
                                                | None -> None
                                                | Some (f, rest) -> Some ((fun e -> f (OpE (Div, e, atom))), rest))
    | _ -> Some ((fun s -> s), ts)

and atom_parser ts =
    match ts with
    | (Number n)::rest -> Some ((NumE n), rest)
    | (Id id)::rest    -> Some ((VarE id), rest)
    | OpenParen::ts -> (match expr_parser ts with
                        | None -> None
                        | Some (exp, rest) -> match rest with
                                              | ClosedParen::rest -> Some (exp, rest)
                                              | _ -> None)
    | _ -> None

let decl_parser ts =
    match ts with
    | (Keyword Let)::(Id id)::Equal::ts -> begin match expr_parser ts with
                                           | Some (exp, rest) -> Some(SimpDec(id, exp), rest)
                                           | None             -> None
                                           end
    | _                                 -> None

let stmt_parser ts =
    match ts with
    | (Id id)::Equal::ts -> begin match expr_parser ts with
                            | Some (exp, rest) -> Some (AssS(id, exp), rest)
                            | None             -> None
                            end
    | (Keyword Print)::ts -> begin match expr_parser ts with
                             | Some (exp, rest) -> Some (PrintS exp, rest)
                             | None             -> None
                             end
                             


let instruction_parser ts =
    match decl_parser ts with
    | Some(decl, rest) -> Some ((Decl decl), rest)
    | None -> match stmt_parser ts with
              | Some(stmt, rest) -> Some ((Stmt stmt), rest)
              | None -> match expr_parser ts with
                        | Some(exp, rest) -> Some (Expr exp, rest)
                        | None -> None

let rec program_parser_list ts acc =
    match instruction_parser ts with
    | None -> acc
    | Some(instr, rest) -> match rest with
                           | Colon::rest -> program_parser_list rest (instr::acc)
                           | _ -> acc

let program_parser ts =
    List.rev (program_parser_list ts [])
