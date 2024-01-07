open Syntax

type 'a pars = token list -> ('a * token list) option

let eps (res : 'a) : 'a pars =
    (fun ts -> Some (res, ts))

let const (t : token) (res : 'a) : 'a pars = function
    | t::ts -> Some(res, ts)
    | _     -> None

let kword kw = 
    const (Keyword kw) (Keyword kw)

let symbol (t : token) : token pars =
    const t t

let (<|>) (p1 : 'a pars) (p2 : 'a pars) : 'a pars = (fun ts ->
    match p1 ts with
    | Some x -> Some x
    | None   -> begin match p2 ts with
                | Some x -> Some x
                | None   -> None
                end)

let rec many (p : 'a pars) : ('a list) pars =
    (fun ts -> match p ts with
               | None        -> Some([], ts)
               | Some(r, ts) -> let Some (rs, ts) = many p ts in
                                    Some (r::rs, ts))

let ( >> ) (p1 : 'a pars) (f : 'a -> 'b) : 'b pars = 
    (fun ts -> match p1 ts with
               | None        -> None
               | Some(r, ts) -> Some(f r, ts))

let liftA2 (p1 : 'a pars) (p2 : 'b pars) (f : 'a -> 'b -> 'c) : 'c pars =
    (fun ts -> match p1 ts with
               | None -> None
               | Some (r1, ts) -> begin match p2 ts with
                                  | None -> None
                                  | Some(r2, ts) -> Some((f r1 r2), ts)
                                  end)

let liftA3 (p1 : 'a pars) (p2 : 'b pars) (p3 : 'c pars) (f : 'a -> 'b -> 'c -> 'd) : 'd pars =
    liftA2 (liftA2 p1 p2 f) p3 (fun k x -> k x)

let liftA4 p1 p2 p3 p4 f =
    liftA2 (liftA3 p1 p2 p3 f) p4 (fun k x -> k x)
    
let liftA5 p1 p2 p3 p4 p5 f =
    liftA2 (liftA4 p1 p2 p3 p4 f) p5 (fun k x -> k x)

let liftA6 p1 p2 p3 p4 p5 p6 f =
    liftA2 (liftA5 p1 p2 p3 p4 p5 f) p6 (fun k x -> k x)
    
let (++) (p1 : 'a pars) (p2 : 'b pars) : ('a * 'b) pars = 
    liftA2 p1 p2 (fun x y -> (x, y))
