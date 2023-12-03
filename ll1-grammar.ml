(* type 't is for terminals, 'n for nonterminals *)
type ('t, 'n) sentencial-form =
    (('t, 'n) Either.t) list

type ('t, 'n) rule =
    't * ('t, 'n) sentencial-form list

type ('t, 'n) ll1-grammar =
    (('t, 'n) rule) list
