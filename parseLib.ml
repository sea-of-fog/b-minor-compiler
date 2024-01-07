open Syntax

type 'a pars = token list -> ('a * token list) option

let eps (res : 'a) : 'a pars =
    (fun ts -> Some (res, ts))

let const (t : token) (res : 'a) : 'a pars = function
    | t::ts -> Some(res, ts)
    | _     -> None

let (++) (p1 : 'a pars) (p2 : 'b pars) : ('a * 'b) pars = 
    (fun ts -> match p1 ts with
               | None -> None
               | Some (r1, ts) -> begin match p2 ts with
                                  | None -> None
                                  | Some(r2, ts) -> Some((r1, r2), ts)
                                  end)

let (<|>) (p1 : 'a pars) (p2 : 'a pars) : 'a pars = (fun ts ->
    match p1 ts with
    | Some x -> Some x
    | None   -> begin match p2 ts with
                | Some x -> Some x
                | None   -> None
                end)

let rec many (p : 'a pars) : 'a list pars =
    (fun ts -> match p ts with
               | None        -> Some([], ts)
               | Some(r, ts) -> let Some (rs, ts) = many p ts in
                                    Some (r::rs, ts))

let ( >> ) (p1 : 'a pars) (f : 'a -> 'b) : 'b pars = 
    (fun ts -> match p1 ts with
               | None        -> None
               | Some(r, ts) -> Some(f r, ts))
