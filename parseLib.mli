open Syntax

type 'a pars = token list -> ('a * token list) option

val eps     : 'a -> 'a pars
val const   : token -> 'a -> 'a pars
val symbol  : token -> token pars
val kword   : keyword -> token pars
val (<|>)   : 'a pars -> 'a pars -> 'a pars
val (++)    : 'a pars -> 'b pars -> ('a * 'b) pars
val (>>)    : 'a pars -> ('a -> 'b) -> 'b pars
val many    : 'a pars -> ('a list) pars
val liftA2  : 'a pars -> 'b pars -> ('a -> 'b -> 'c) -> 'c pars
val liftA3  : 'a pars -> 'b pars -> 'c pars -> ('a -> 'b -> 'c -> 'd) -> 'd pars
val liftA4  : 'a pars -> 'b pars -> 'c pars -> 'd pars -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e pars
val liftA5  : 'a pars -> 'b pars -> 'c pars -> 'd pars -> 'e pars -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'f pars
val liftA6  : 'a pars -> 'b pars -> 'c pars -> 'd pars -> 'e pars -> 'f pars -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'g pars
