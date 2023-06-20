type op = add | sub | mul | div

type expr =
    | NumberE of int
    | OpE of op * int * int
