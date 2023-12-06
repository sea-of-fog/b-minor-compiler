(* data structure storing lines of code *)

type t

val empty       : t
val single_line : string -> t

(* Add line to the end of code *)
val prefix      : string -> t -> t
val add_line    : string -> t -> t
val concat      : t -> t -> t
val from_list   : string list -> t

val to_lines    : t -> string list
